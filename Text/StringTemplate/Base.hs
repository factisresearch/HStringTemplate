{-# OPTIONS -O2 -fbang-patterns -fglasgow-exts -fno-monomorphism-restriction #-}
{-# OPTIONS_HADDOCK not-home #-}

module Text.StringTemplate.Base
    (StringTemplate, StringTemplateShows(..), ToSElem(..), STGen,
     Stringable(..),
     toString, toPPDoc, render, newSTMP, newAngleSTMP, getStringTemplate,
     setAttribute, setManyAttrib,
     groupStringTemplates, addSuperGroup, addSubGroup,
     mergeSTGroups, stringTemplateFileGroup, cacheSTGroup, cacheSTGroupForever,
     optInsertGroup, optInsertTmpl, paddedTrans
    ) where
import Control.Monad
import Control.Arrow hiding (pure)
import Control.Applicative hiding ((<|>),many)
import Data.Maybe
import Data.Monoid
import Data.List
import Text.ParserCombinators.Parsec
import System.Time
import System.IO
import System.FilePath
import Data.IORef
import System.IO.Unsafe
import qualified Data.Map as M
import qualified Text.PrettyPrint.HughesPJ as PP

import Text.StringTemplate.Classes
--import Debug.Trace --DEBUG
--import Text.StringTemplate.Instances()

{--------------------------------------------------------------------
  Generic Utilities
--------------------------------------------------------------------}

instance Applicative (GenParser tok st) where pure = return; (<*>) = ap;

o :: (b -> c) -> (a -> a1 -> b) -> a -> a1 -> c
o = (.).(.)
infixr 8 `o`

(|.) :: (t1 -> t2) -> (t -> t1) -> t -> t2
(|.) f g x = f (g x)
infixr 3 |.

(.>>) :: (Monad m) => m a -> m b -> m b
(.>>) f g = f >> g
infixr 5 .>>

(<$$>) :: (Functor f1, Functor f) => (a -> b) -> f (f1 a) -> f (f1 b)
(<$$>) x y = ((<$>) . (<$>)) x y

fromMany :: b -> ([a] -> b) -> [a] -> b
fromMany e _ [] = e
fromMany _ f xs  = f xs

swing :: (((a -> c1) -> c1) -> b -> c) -> b -> a -> c
swing = flip . (. flip id)

paddedTrans :: a -> [[a]] -> [[a]]
paddedTrans _ [] = []
paddedTrans n as = take (maximum . map length $ as) . trans $ as
    where trans [] = [];
          trans ([] : xss)  = (n : map h xss) :  trans ([n] : (map t xss))
          trans ((x : xs) : xss) = (x : map h xss) : trans (m xs : (map t xss))
          h (x:_) = x; h _ = n; t (_:y:xs) = (y:xs); t _ = [n];
          m (x:xs) = (x:xs); m _ = [n];


{--------------------------------------------------------------------
  StringTemplate and the API
--------------------------------------------------------------------}
--add noInline to unsafe methods?

-- | A function that generates StringTemplates.
-- | This is conceptually a query function into a \"group\" of StringTemplates.
type STGen a = String -> (StFirst (StringTemplate a))

-- | A String with \"holes\" in it. StringTemplates may be composed of any
-- "Stringable" type, which at the moment includes 'String's, 'ByteString's,
-- PrettyPrinter 'Doc's, and 'Endo' 'String's, which are actually of type
-- 'ShowS'. When a StringTemplate is composed of a type, its internals are
-- as well, so it is, so to speak \"turtles all the way down.\"
data StringTemplate a = STMP {senv :: SEnv a,  runSTMP :: SEnv a -> a}

-- | Renders a StringTemplate to a String.
toString :: StringTemplate String -> String
toString = render

-- | Renders a StringTemplate to a HughesPJ Pretty Printer Doc.
toPPDoc :: StringTemplate PP.Doc -> PP.Doc
toPPDoc = render

-- | Generic render function for a StringTemplate of any type.
render :: Stringable a => StringTemplate a -> a
render = runSTMP <*> senv

-- | Parses a String to produce a StringTemplate, with \'$\'s as delimiters.
-- It is constructed with a stub group that cannot look up other templates.
newSTMP :: Stringable a => String -> StringTemplate a
newSTMP = STMP (SEnv M.empty [] mempty) . parseSTMP ('$','$')

-- | Parses a String to produce a StringTemplate, delimited by \'<\' and \'>\'.
-- It is constructed with a stub group that cannot look up other templates.
newAngleSTMP :: Stringable a => String -> StringTemplate a
newAngleSTMP = STMP (SEnv M.empty [] mempty) . parseSTMP ('$','$')

-- | Yields a StringTemplate with the appropriate attribute set.
-- If the attribute already exists, it is appended to a list.
setAttribute :: (ToSElem a, Stringable b) => String -> a -> StringTemplate b -> StringTemplate b
setAttribute s x st = st {senv = envInsApp s (toSElem x) (senv st)}

-- | Yields a StringTemplate with the appropriate attributes set.
-- If any attribute already exists, it is appended to a list.
setManyAttrib :: (ToSElem a, Stringable b) => [(String, a)] -> StringTemplate b -> StringTemplate b
setManyAttrib = flip . foldl' . flip $ uncurry setAttribute

-- | Queries an SGen and returns Just the appropriate StringTemplate if it exists,
-- otherwise, Nothing.
getStringTemplate :: (Stringable a) => String -> STGen a -> Maybe (StringTemplate a)
getStringTemplate s sg = stGetFirst (sg s)

-- | Given a list of named of StringTemplates, returns a group which generates
-- them such that they can call one another.
groupStringTemplates :: [(String,StringTemplate a)] -> STGen a
groupStringTemplates xs = newGen
    where newGen s = StFirst (M.lookup s ng)
          ng = foldl' (flip $ uncurry M.insert) M.empty $
               map (second $ sgInsert newGen) xs

-- | Given a path, returns a group which generates all files in said directory.
stringTemplateFileGroup :: Stringable a => String -> STGen a
stringTemplateFileGroup path = stfg
    where stfg = StFirst . Just . STMP (SEnv M.empty [] stfg) .
                 parseSTMP ('$', '$') . unsafePerformIO . readFile . (path </>)

-- | Adds a set of global options to a group
optInsertGroup :: [(String, String)] -> STGen a -> STGen a
optInsertGroup opts f = optInsertTmpl opts <$$> f

-- | Adds a set of global options to a single template
optInsertTmpl :: [(String, String)] -> StringTemplate a -> StringTemplate a
optInsertTmpl x st = st {senv = optInsert (map (second justSTR) x) (senv st)}

-- | Adds a supergroup to any StringTemplate group such that templates from
-- the original group are now able to call ones from the supergroup as well.
addSuperGroup :: STGen a -> STGen a -> STGen a
addSuperGroup f g = sgInsert g <$$> f

-- | Adds a \"subgroup\" to any StringTemplate group such that templates from
-- the original group now have template calls \"shadowed\" by the subgroup.
addSubGroup :: STGen a -> STGen a -> STGen a
addSubGroup f g = sgOverride g <$$> f

-- | Merges two groups into a single group. This function is left-biased,
-- prefering bindings from the first group when there is a conflict.
mergeSTGroups :: STGen a -> STGen a -> STGen a
mergeSTGroups f g = addSuperGroup f g `mappend` addSubGroup g f

-- | Given an integral amount of seconds and a group, returns a group cached for
-- that span of time. Does not cache \"misses.\"
cacheSTGroup :: Int -> STGen a -> STGen a
cacheSTGroup m g = unsafePerformIO $ go <$> newIORef M.empty
    where go r s = unsafePerformIO $ do
                     mp <- readIORef r
                     now <- getClockTime
                     maybe (udReturn now)
                      (\(t, st) -> if (tdSec . normalizeTimeDiff $
                           diffClockTimes now t) > m
                         then udReturn now
                         else return st)
                      . M.lookup s $ mp
              where udReturn now = maybe (return found)
                                   (const $ do
                                      atomicModifyIORef r $
                                         flip (,) () . M.insert s (now, found)
                                      return found)
                                     . stGetFirst $ found
                        where found = g s

-- | Returns a group cached forever. Caches \"misses\" as well as hits.
cacheSTGroupForever :: STGen a -> STGen a
cacheSTGroupForever g = unsafePerformIO $ go <$> newIORef M.empty
    where go r s = unsafePerformIO $ do
                     mp <- readIORef r
                     maybe udReturn
                      return . M.lookup s $ mp
              where udReturn = do
                                let st = g s
                                atomicModifyIORef r $
                                  flip (,) () . M.insert s st
                                return st

{--------------------------------------------------------------------
  Internal API
--------------------------------------------------------------------}
--IMPLEMENT groups having stLookup return a Maybe for regions

data SEnv a = SEnv {smp :: SMap a, sopts :: [(String, (SEnv a -> SElem a))], sgen :: STGen a}

envLookup :: (Monad m) => String -> SEnv a -> m (SElem a)
envLookup x = M.lookup x . smp
envInsert :: (String, SElem a) -> SEnv a -> SEnv a
envInsert (s, x) y = y {smp = M.insert s x (smp y)}
envInsApp :: Stringable a => String -> SElem a -> SEnv a -> SEnv a
envInsApp  s  x  y = y {smp = M.insertWith go s x (smp y)}
    where go a (LI bs) = LI (a:bs)
          go a b = LI [a,b]

optLookup :: String -> SEnv a -> Maybe (SEnv a -> SElem a)
optLookup x = lookup x . sopts
optInsert :: [(String, SEnv a -> SElem a)] -> SEnv a -> SEnv a
optInsert x env = env {sopts = x ++ sopts env}
nullOpt :: SEnv a -> SElem a
nullOpt = fromMaybe (justSTR "") =<< optLookup "null"

stLookup :: (Stringable a) => [Char] -> SEnv a -> StringTemplate a
stLookup x env = maybe (newSTMP ("No Template Found for: " ++ x))
                 (\st-> st {senv = env}) $ stGetFirst (sgen env x)

sgInsert :: (String -> StFirst (StringTemplate a)) -> StringTemplate a -> StringTemplate a
sgInsert   g st = let e = senv st in st {senv = e {sgen = sgen e `mappend` g} }
sgOverride :: STGen a -> StringTemplate a -> StringTemplate a
sgOverride g st = let e = senv st in st {senv = e {sgen = g `mappend` sgen e} }

parseSTMP :: (Stringable a) => (Char, Char) -> String -> SEnv a -> a
parseSTMP x = either (showStr .  show) (id) . runParser (stmpl False) x ""

{--------------------------------------------------------------------
  Internal API for polymorphic display of elements
--------------------------------------------------------------------}

showVal :: Stringable a => SEnv a -> SElem a -> a
showVal snv se =
    case se of (STR x) -> stFromString x
               (LI xs) -> joinUp xs
               (SM sm) -> joinUp $ M.elems sm
               (STSH x) -> stFromString (format x)
               (SBLE x) -> x
               SNull -> showVal <*> nullOpt $ snv
    where sepVal = fromMaybe (justSTR "") =<< optLookup "separator" $ snv
          format = maybe stshow . stfshow <*> optLookup "format" $ snv
          joinUp = mintercalate (showVal snv sepVal) . map (showVal snv)

showStr :: Stringable a => String -> SEnv a -> a
showStr = flip showVal . STR

{--------------------------------------------------------------------
  Utility Combinators
--------------------------------------------------------------------}

justSTR :: String -> b -> SElem a
justSTR = const . STR
stshow :: STShow -> String
stshow (STShow a) = stringTemplateShow a
stfshow :: Stringable a => SEnv a -> (SEnv a -> SElem a) -> STShow -> String
stfshow snv fs (STShow a) = stringTemplateFormattedShow
                            (stToString `o` showVal <*> fs $ snv) a

around :: Char -> GenParser Char st t -> Char -> GenParser Char st t
around x p y = do {char x; v<-p; char y; return v}
spaced :: GenParser Char st t -> GenParser Char st t
spaced p = do {spaces; v<-p; spaces; return v}
word :: GenParser Char st [Char]
word = many1 alphaNum
comlist :: GenParser Char st a -> GenParser Char st [a]
comlist p = spaced (p `sepBy1` spaced (char ','))

props :: Stringable a => GenParser Char (Char, Char) [SEnv a -> SElem a]
props = many $ char '.' >> (around '(' subexprn ')' <|> justSTR <$> word)

escapedChar, escapedStr :: [Char] -> GenParser Char st [Char]
escapedChar chs =
    noneOf chs >>= \x -> if x == '\\' then anyChar >>= \y ->
    if y `elem` chs then return [y] else return [x, y] else return [x]
escapedStr chs = concat <$> many1 (escapedChar chs)

{--------------------------------------------------------------------
  The Grammar
--------------------------------------------------------------------}

-- | if p is true, stmpl can fail gracefully, false it dies hard.
-- Set to false at the top level, and true within if expressions.
stmpl :: Stringable a => Bool -> GenParser Char (Char,Char) (SEnv a -> a)
stmpl p = do
  (ca, cb) <- getState
  mconcat <$> many (showStr <$> escapedStr [ca] <|> try (around ca optExpr cb)
                    <|> try comment <|> bl <?> "template")
      where bl | p = try blank | otherwise = blank

subStmp :: Stringable a => GenParser Char (Char,Char) (([SElem a], [SElem a]) -> SEnv a -> a)
subStmp = do
  (ca, cb) <- getState
  udEnv <- option (transform ["it"]) (transform <$> try attribNames)
  st <- mconcat <$> many (showStr <$> escapedStr (ca:"}|")
                         <|> try (around ca optExpr cb)
                         <|> try comment <|> blank  <?> "subtemplate")
  return (st `o` udEnv)
      where transform an (att,i:i0:[]) =
                flip (foldr envInsert) $ zip ("i":"i0":an) (i:i0:att)
            attribNames = (char '|' >>) . return =<< comlist (spaced word)

comment :: Stringable a => GenParser Char (Char,Char) (SEnv a -> a)
comment = do
  (ca, cb) <- getState
  string (ca:'!':[]) >> manyTill anyChar (try . string $ '!':cb:[])
  return (showStr "")

blank :: Stringable a => GenParser Char (Char,Char) (SEnv a -> a)
blank = do
  (ca, cb) <- getState
  char ca
  spaces
  char cb
  return (showStr "")

optExpr :: Stringable a => GenParser Char (Char,Char) (SEnv a -> a)
optExpr = do
  (_, cb) <- getState
  (try (string ("else"++[cb])) <|> try (string "elseif(") <|>
    try (string "endif")) .>> fail "Malformed If Statement." <|> return ()
  expr <- try stat <|> spaced exprn
  opts <- many opt
  skipMany (char ';')
  return $ expr |. optInsert opts
      where opt = around ';' (spaced word) '=' >>= (<$> spaced subexprn) . (,)

{--------------------------------------------------------------------
  Statements
--------------------------------------------------------------------}

getProp :: Stringable a => [SEnv a -> SElem a] -> SElem a -> SEnv a -> SElem a
getProp (p:ps) (SM mp) = maybe SNull . flip (getProp ps) <*>
                          flip M.lookup mp . ap (stToString `o` showVal) p
getProp (_:_) _ = const SNull
getProp _ se = const se

ifIsSet :: t -> t -> Bool -> SElem a -> t
ifIsSet t e n SNull = if n then e else t
ifIsSet t e n _ = if n then t else e

parseif :: Stringable a => Char -> GenParser Char (Char, Char) (Bool, SEnv a -> SElem a, [SEnv a -> SElem a], Char, SEnv a -> a, SEnv a -> a)
parseif cb = (,,,,,) <$> option True (char '!' >> return False) <*> subexprn
             <*> props <*> char ')' .>> char cb <*> stmpl True <*>
             (try elseifstat <|> try elsestat <|> endifstat)

stat ::Stringable a => GenParser Char (Char, Char) (SEnv a -> a)
stat = do
  (_, cb) <- getState
  string "if("
  (n, e, p, _, act, cont) <- parseif cb
  return (ifIsSet act cont n =<< getProp p =<< e)

elseifstat ::Stringable a => GenParser Char (Char, Char) (SEnv a -> a)
elseifstat = do
  (ca, cb) <- getState
  char ca >> string "elseif("
  (n, e, p, _, act, cont) <- parseif cb
  return (ifIsSet act cont n =<< getProp p =<< e)

elsestat ::Stringable a => GenParser Char (Char, Char) (SEnv a -> a)
elsestat = do
  (ca, cb) <- getState
  around ca (string "else") cb
  act <- stmpl True
  char ca >> string "endif"
  return act

endifstat ::Stringable a => GenParser Char (Char, Char) (SEnv a -> a)
endifstat = getState >>= char . fst >> string "endif" >> return (showStr "")

{--------------------------------------------------------------------
  Expressions
--------------------------------------------------------------------}

exprn :: Stringable a => GenParser Char (Char,Char) (SEnv a -> a)
exprn = do
  exprs <- comlist subexprn <?> "expression"
  templ <- many (char ':' >> iterApp <$> comlist (anonTmpl <|> regTemplate))
  return $ fromMany (showVal <*> head exprs)
             ((sequence exprs >>=) . seqTmpls) templ

seqTmpls :: Stringable a => [[SElem a] -> SEnv a -> a] -> [SElem a] -> SEnv a -> a
seqTmpls [f]    y = f y
seqTmpls (f:fs) y = seqTmpls fs =<< (:[]) . SBLE . f y
seqTmpls  _ _     = const (stFromString "")

subexprn :: Stringable a => GenParser Char (Char,Char) (SEnv a -> SElem a)
subexprn = cct <$> spaced
            (braceConcat
             <|> SBLE `o` ($ ([SNull],ix0)) <$> try regTemplate
             <|> attrib
             <|> SBLE `o` ($ ([SNull],ix0)) <$> anonTmpl
             <?> "expression")
           `sepBy1` spaced (char '+')
    where cct xs@(_:_:_) = SBLE |.
                           flip mconcatMap <$> showVal <*> sequence xs
          cct [x] = x
          cct  _  = const SNull

braceConcat :: Stringable a => GenParser Char (Char,Char) (SEnv a -> SElem a)
braceConcat = LI . foldr go [] `o` sequence <$> around '['(comlist subexprn)']'
    where go (LI x) lst = x++lst; go x lst = x:lst


literal :: GenParser Char st (b -> SElem a)
literal = justSTR <$> (around '"' (concat <$> many (escapedChar "\"")) '"'
                       <|> around '\'' (concat <$> many (escapedChar "'")) '\'')

attrib :: Stringable a => GenParser Char (Char,Char) (SEnv a -> SElem a)
attrib = do
  a <- literal <|> try functn <|> prepExp <$> word <|> around '(' subexprn ')'
         <?> "attribute"
  proprs <- props
  return $ fromMany a ((a >>=) . getProp) proprs
      where prepExp var = fromMaybe SNull <$> envLookup var

functn :: Stringable a => GenParser Char (Char,Char) (SEnv a -> SElem a)
functn = do
  f <- string "first" <|> string "rest" <|> string "strip"
       <|> try (string "length") <|> string "last"
  (fApply f .) <$> around '(' subexprn ')'
      where fApply str (LI xs)
                | str == "first"  = head xs
                | str == "last"   = last xs
                | str == "rest"   = (LI . tail) xs
                | str == "strip"  = LI . filter (not . liNil) $ xs
                | str == "length" = STR . show . length $ xs
            fApply str x
                | str == "rest"   = (LI [])
                | str == "length" = STR "1"
                | otherwise       = x
            liNil (LI x) = null x
            liNil _ = False

{--------------------------------------------------------------------
  Templates
--------------------------------------------------------------------}
--change makeTmpl to do notation for clarity?

mkIndex :: Num b => [b] -> [[SElem a]]
mkIndex = map ((:) . STR . show . (1+) <*> (:[]) . STR . show)
ix0 :: [SElem a]
ix0 = [STR "1",STR "0"]

cycleApp :: (Stringable a) => [([SElem a], [SElem a]) -> SEnv a -> a] -> [([SElem a], [SElem a])]  -> SEnv a -> a
cycleApp x y = mconcatMap (zipWith ($) (cycle x) y) . flip ($)

pluslen :: [a] -> [([a], [SElem b])]
pluslen xs = zip (map (:[]) xs) $ mkIndex [0..(length xs)]

liTrans :: [SElem a] -> [([SElem a], [SElem a])]
liTrans = pluslen' . paddedTrans SNull . map u
    where u (LI x) = x; u x = [x]
          pluslen' xss@(x:_) = zip xss $ mkIndex [0..(length x)]

iterApp :: Stringable a => [([SElem a], [SElem a]) -> SEnv a -> a] -> [SElem a] -> SEnv a -> a
iterApp [f] (LI xs:[]) = (mconcatMap $ pluslen xs) . flip f
iterApp [f] vars@(LI _:_) = (mconcatMap $ liTrans vars) . flip f
iterApp [f] v = f (v,ix0)
iterApp fs (LI xs:[]) = cycleApp fs (pluslen xs)
iterApp fs vars@(LI _:_) = cycleApp fs (liTrans vars)
iterApp fs xs = cycleApp fs (pluslen xs)

anonTmpl :: Stringable a => GenParser Char (Char, Char) (([SElem a], [SElem a]) -> SEnv a -> a)
anonTmpl = around '{' subStmp '}'

regTemplate :: Stringable a => GenParser Char (Char, Char) (([SElem a], [SElem a]) -> SEnv a -> a)
regTemplate = do
  try (functn::GenParser Char (Char,Char) (SEnv String -> SElem String)) .>> fail "" <|> return ()
  name <- justSTR <$> word <|> around '(' subexprn ')'
  vals <- around '(' (spaced $ try assgn <|> anonassgn <|> return []) ')'
  return $ join . (. name) . makeTmpl vals
      where makeTmpl v ((se:_),is) (STR x)  =
                render |. stBind . (zip ["it","i","i0"] (se:is) ++)
                           . swing (map . second) v <*> stLookup x
            makeTmpl _ _ _ = showStr "Invalid Template Specified"
            stBind v st = st {senv = foldr envInsert (senv st) v}
            anonassgn = (:[]) . (,) "it" <$> subexprn
            assgn = (spaced word >>= (<$> char '=' .>> spaced subexprn) . (,))
                    `sepEndBy1` char ';'

{- DEBUG

pTrace s = pt <|> return ()
    where pt = try $
               do
                 x <- try $ many1 anyChar
                 trace (s++": " ++x) $ try $ char 'z'
                 fail x
-}
