{-# OPTIONS -O2 -fbang-patterns -fglasgow-exts -fno-monomorphism-restriction #-}

module Text.StringTemplate.Base
    (StringTemplate, toString, toByteString, toShowS, toPPDoc,
     newSTMP, newAngleSTMP, StringTemplateShows(..), ToSElem(..), STGen,
     setAttribute, groupStringTemplates, addSuperGroup, addSubGroup,
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
import qualified Data.ByteString.Char8 as B
import qualified Text.PrettyPrint.HughesPJ as PP

import Text.StringTemplate.Classes
import Debug.Trace --DEBUG
import Text.StringTemplate.Instances -- DEBUG


{--------------------------------------------------------------------
  Generic Utilities
--------------------------------------------------------------------}

instance Applicative (GenParser tok st) where pure = return; (<*>) = ap;

o = (.).(.)
infixr 8 `o`

(|.) f g x = f (g x)
infixr 3 |.

(.>>) f g = f >> g
infixr 5 .>>

(<$$>) x y = ((<$>) . (<$>)) x y

fromMany e f [] = e
fromMany e f xs  = f xs

intercalate = concat `o` intersperse
swing = flip . (. flip id)

paddedTrans n [] = []
paddedTrans n xs = take (maximum . map length $ xs) . trans $ xs
    where trans [] = [];
          trans ([] : xss)  = (n : map h xss) :  trans ([n] : (map t xss))
          trans ((x : xs) : xss) = (x : map h xss) : trans (m xs : (map t xss))
          h (x:xs) = x; h _ = n; t (x:y:xs) = (y:xs); t _ = [n];
          m (x:xs) = (x:xs); m _ = [n];

{--------------------------------------------------------------------
  StringTemplate and the API
--------------------------------------------------------------------}
--add noInline to unsafe methods?

-- | A Function that generates StringTemplates
type STGen = String -> (First (StringTemplate))

-- | A String with \"holes\" in it.
data StringTemplate = STMP {senv :: SEnv,  runSTMP :: forall a. Stringable a => SEnv->a}

-- | Renders a StringTemplate to a String.
toString :: StringTemplate -> String
toString = render

-- | Renders a StringTemplate to a String.
toByteString :: StringTemplate -> B.ByteString
toByteString = render

-- | Renders a StringTemplate to a ShowS.
toShowS :: StringTemplate -> ShowS
toShowS = appEndo |. render

-- | Renders a StringTemplate to a HughesPJ Pretty Printer Doc.
toPPDoc :: StringTemplate -> PP.Doc
toPPDoc = render

-- | Parses a String to produce a StringTemplate, with '$'s as delimiters.
-- It is constructed with a stub group that cannot look up other templates.
newSTMP :: String -> StringTemplate
newSTMP s = STMP (SEnv M.empty [] mempty) (\env -> parseSTMP ('$','$') s env)

-- | Parses a String to produce a StringTemplate, delimited by '<' and '>'.
-- It is constructed with a stub group that cannot look up other templates.
newAngleSTMP :: String -> StringTemplate
newAngleSTMP s = STMP (SEnv M.empty [] mempty) (\env -> parseSTMP ('$','$') s env)

-- | Yields a StringTemplate with the appropriate attribute set.
-- If the attribute already exists, it is appended to a list.
setAttribute :: (ToSElem a) => String -> a -> StringTemplate -> StringTemplate
setAttribute s x st = st {senv = envInsApp s (toSElem x) (senv st)}

-- | Given a list of named of StringTemplates, returns a group which generates
-- them such that they can call one another.
groupStringTemplates :: [(String,StringTemplate)] -> STGen
groupStringTemplates xs = newGen
    where newGen s = First (M.lookup s ng)
          ng = foldl' (flip $ uncurry M.insert) M.empty $
               map (second $ sgInsert newGen) xs

-- | Given a path, returns a group which generates all files in said directory.
stringTemplateFileGroup :: String -> STGen
stringTemplateFileGroup path = stfg
    where stfg s = First . Just $ STMP (SEnv M.empty [] stfg)
                   (parseSTMP ('$','$')
                    (unsafePerformIO . readFile $ path </> s))

-- | Adds a set of global options to a group
optInsertGroup :: [(String, String)] -> STGen -> STGen
optInsertGroup opts f = optInsertTmpl (map (second justSTR) opts) <$$> f

-- | Adds a supergroup to any StringTemplate group such that templates from
-- the original group are now able to call ones from the supergroup as well.
addSuperGroup :: STGen -> STGen -> STGen
addSuperGroup f g = sgInsert g <$$> f

-- | Adds a "subgroup" to any StringTemplate group such that templates from
-- the original group now have template calls "shadowed" by the subgroup.
addSubGroup :: STGen -> STGen -> STGen
addSubGroup f g = sgOverride g <$$> f

-- | Merges two groups into a single group. This function is left-biased,
-- prefering bindings from the first group when there is a conflict.
mergeSTGroups :: STGen -> STGen -> STGen
mergeSTGroups f g = addSuperGroup f g `mappend` addSubGroup g f

-- | Given an integral amount of seconds and a group, returns a group cached for
-- that span of time. Does not cache "misses."
cacheSTGroup :: Int -> STGen -> STGen
cacheSTGroup m g = unsafePerformIO $ go <$> newIORef M.empty
    where go r s = unsafePerformIO $ do
                     mp <- readIORef r
                     now <- getClockTime
                     maybe (udReturn mp now)
                      (\(t, st) -> if (tdSec . normalizeTimeDiff $
                           diffClockTimes now t) > m
                         then udReturn mp now
                         else return st)
                      . M.lookup s $ mp
              where udReturn mp now = maybe (return found)
                                      (\st -> do
                                         atomicModifyIORef r $
                                          flip (,) () . M.insert s (now, found)
                                         return found)
                                      . getFirst $ found
                        where found = g s

-- | Returns a group cached forever. Caches "misses" as well as hits.
cacheSTGroupForever :: STGen -> STGen
cacheSTGroupForever g = unsafePerformIO $ go <$> newIORef M.empty
    where go r s = unsafePerformIO $ do
                     mp <- readIORef r
                     maybe (udReturn mp)
                      return . M.lookup s $ mp
              where udReturn mp = do
                                  let st = g s
                                  atomicModifyIORef r $
                                   flip (,) () . M.insert s st
                                  return st

{--------------------------------------------------------------------
  Internal API
--------------------------------------------------------------------}
--IMPLEMENT groups having stLookup return a Maybe for regions

render :: Stringable a => StringTemplate -> a
render = runSTMP <*> senv

data SEnv = SEnv {smp :: SMap, sopts :: [(String, SEnv -> SElem)], sgen :: STGen}

envLookup x = M.lookup x . smp
envInsert (s, x) y = y {smp = M.insert s x (smp y)}
envInsApp  s  x  y = y {smp = M.insertWith app s x (smp y)}
    where app x (LI ys) = LI (x:ys)
          app x y = LI [x,y]

optLookup x = lookup x . sopts
optInsert x env = env {sopts = x ++ sopts env}
optInsertTmpl x st = st {senv = optInsert x (senv st)}
nullOpt = fromMaybe (justSTR "") =<< optLookup "null"

stLookup x env = maybe (newSTMP ("No Template Found for: " ++ x))
                 (\st-> st {senv = env}) $ getFirst (sgen env x)

sgInsert   g st = let e = senv st in st {senv = e {sgen = sgen e `mappend` g} }
sgOverride g st = let e = senv st in st {senv = e {sgen = g `mappend` sgen e} }

parseSTMP :: (Stringable a) => (Char, Char) -> String -> SEnv -> a
parseSTMP x = either (showStr .  show) (id) . runParser (stmpl False) x ""

{--------------------------------------------------------------------
  Internal API for polymorphic display of elements
--------------------------------------------------------------------}

defaultShowVal :: SEnv -> SElem -> String
defaultShowVal snv se =
    case se of (STR x)-> x; (LI xs)-> joinUp xs; (SM sm)-> joinUp $ M.elems sm
               (STSH x)-> format x; SNull -> showVal <*> nullOpt $ snv 
    where sepVal = fromMaybe (justSTR "") =<< optLookup "seperator" $ snv
          format = maybe stshow . stfshow <*> optLookup "format" $ snv
          joinUp = intercalate (showVal snv sepVal) . map (showVal snv)

showStr :: Stringable a => String -> SEnv -> a
showStr = flip showVal . STR

ppShowVal snv se =
    case se of (STR x)-> PP.text x;
               (LI xs)-> joinUp xs;
               (SM sm)-> joinUp $ M.elems sm
               (STSH x)-> PP.text (format x);
               SNull -> ppShowVal <*> nullOpt $ snv 
    where sepVal = fromMaybe (justSTR "") =<< optLookup "seperator" $ snv
          format = maybe stshow . stfshow <*> optLookup "format" $ snv
          joinUp = PP.fcat . PP.punctuate (ppShowVal snv sepVal)
                   . map (ppShowVal snv)

class Monoid a => Stringable a where
    stFromString :: String -> a
    stToString :: a -> String
    showVal :: SEnv -> SElem -> a
    showVal = stFromString `o` defaultShowVal
    mconcatMap :: [b] -> (b -> a) -> a
    mconcatMap m k = foldr (mappend . k) mempty m

instance Stringable String where
    stFromString = id
    stToString = id

instance Stringable PP.Doc where
    stFromString = PP.text
    stToString = PP.render
    showVal = ppShowVal
    mconcatMap m k = PP.fcat . map k $ m
   
instance Monoid PP.Doc where
    mempty = PP.empty
    x `mappend` y = x PP.<> y

instance Stringable B.ByteString where
    stFromString = B.pack
    stToString = B.unpack

instance Stringable (Endo String) where
    stFromString = Endo . (++) 
    stToString = ($ []) . appEndo

{--------------------------------------------------------------------
  Utility Combinators
--------------------------------------------------------------------}

justSTR = const . STR
stshow (STShow a) = stringTemplateShow a
stfshow snv fs (STShow a) = stringTemplateFormattedShow (showVal <*> fs $ snv) a

around x p y = do {char x; v<-p; char y; return v}
spaced p = do {spaces; v<-p; spaces; return v}
word = many1 alphaNum
comlist p = spaced (p `sepBy1` spaced (char ','))
props = many $ char '.' >> (around '(' subexprn ')' <|> justSTR <$> word)

escapedChar chs =
    noneOf chs >>= \x -> if x == '\\' then anyChar >>= \y ->
    if y `elem` chs then return [y] else return [x, y] else return [x]
escapedStr chs = concat <$> many1 (escapedChar chs)

{--------------------------------------------------------------------
  The Grammar
--------------------------------------------------------------------}

-- | if p is true, stmpl can fail gracefully, false it dies hard.
-- Set to false at the top level, and true within if expressions.
stmpl :: Stringable a => Bool -> GenParser Char (Char,Char) (SEnv -> a)
stmpl p = do
  (ca, cb) <- getState
  mconcat <$> many (showStr <$> escapedStr [ca] <|> try (around ca optExpr cb) 
                    <|> try comment <|> bl <?> "template")
      where bl | p = try blank | otherwise = blank

subStmp :: Stringable a => GenParser Char (Char,Char) (([SElem], [SElem]) -> SEnv -> a)
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

comment :: Stringable a => GenParser Char (Char,Char) (SEnv -> a)
comment = do
  (ca, cb) <- getState
  string (ca:'!':[]) >> manyTill anyChar (try . string $ '!':cb:[])
  return (showStr "")

blank :: Stringable a => GenParser Char (Char,Char) (SEnv -> a)
blank = do
  (ca, cb) <- getState
  char ca
  spaces
  char cb
  return (showStr "")

optExpr :: Stringable a => GenParser Char (Char,Char) (SEnv -> a)
optExpr = do
  (ca, cb) <- getState
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

getProp (p:ps) (SM mp) = maybe SNull . flip (getProp ps) <*>
                          flip M.lookup mp . ap showVal p
getProp (p:ps) _ = const SNull
getProp _ se = const se

ifIsSet t e n SNull = if n then e else t
ifIsSet t e n _ = if n then t else e

parseif cb = (,,,,,) <$> option True (char '!' >> return False) <*> subexprn 
             <*> props <*> char ')' .>> char cb <*> stmpl True <*>
             (try elseifstat <|> try elsestat <|> endifstat)

stat ::Stringable a => GenParser Char (Char, Char) (SEnv -> a)
stat = do
  (ca, cb) <- getState
  string "if("
  (n, e, p, _, act, cont) <- parseif cb
  return (ifIsSet act cont n =<< getProp p =<< e)

elseifstat ::Stringable a => GenParser Char (Char, Char) (SEnv -> a)
elseifstat = do
  (ca, cb) <- getState
  char ca >> string "elseif("
  (n, e, p, _, act, cont) <- parseif cb
  return (ifIsSet act cont n =<< getProp p =<< e)

elsestat ::Stringable a => GenParser Char (Char, Char) (SEnv -> a)
elsestat = do
  (ca, cb) <- getState
  around ca (string "else") cb
  act <- stmpl True
  char ca >> string "endif"
  return act

endifstat ::Stringable a => GenParser Char (Char, Char) (SEnv -> a)
endifstat = getState >>= char . fst >> string "endif" >> return (showStr "")

{--------------------------------------------------------------------
  Expressions
--------------------------------------------------------------------}
--seqTmpls is sort of messy, with the lifting and unlifting.
--subexprn needs to return an SElem but we can clean it up still.
--we need to fix this to get application to nested templates right
--with prettyprinting. only decent way to do this seems to be
--to add a new constructor to SElem holding an existential showable

exprn :: Stringable a => GenParser Char (Char,Char) (SEnv -> a)
exprn = do
  exprs <- comlist subexprn <?> "expression"
  templ <- many (char ':' >> iterApp <$> comlist (anonTmpl <|> regTemplate))
  return $ fromMany (showVal <*> head exprs)
             ((sequence exprs >>=) . seqTmpls) templ

seqTmpls :: Stringable a => [[SElem] -> SEnv -> a] -> [SElem] -> SEnv -> a
seqTmpls [f]    y = f y
seqTmpls (f:fs) y = seqTmpls fs =<< (:[]) . STR . stToString . f y

subexprn :: GenParser Char (Char,Char) (SEnv -> SElem)
subexprn = cct <$> spaced
            (braceConcat <|> STR `o` ($ ([SNull],ix0)) <$> try regTemplate <|>
             attrib <|> STR `o` ($ ([SNull],ix0)) <$> anonTmpl <?> "expression")
           `sepBy1` spaced (char '+')
    where cct xs@(x:y:z) = STR . (concatMap <$> showVal <*> sequence xs)
          cct (x:xs) = x
 
braceConcat = LI . foldr go [] `o` sequence <$> around '['(comlist subexprn)']'
    where go (LI x) lst = x++lst; go x lst = x:lst

literal = justSTR <$> (around '"' (concat <$> many (escapedChar "\"")) '"'
                       <|> around '\'' (concat <$> many (escapedChar "'")) '\'')

attrib = do
  a <- literal <|> try functn <|> prepExp <$> word <|> around '(' subexprn ')'
         <?> "attribute"
  proprs <- props
  return $ fromMany a ((a >>=) . getProp) proprs
      where prepExp var = fromMaybe SNull <$> envLookup var

functn = do
  f <- string "first" <|> string "rest" <|> string "strip"
       <|> try (string "length") <|> string "last"
  (fApply f .) <$> around '(' subexprn ')'
      where fApply str (LI xs)
                | str == "first"  = head xs
                | str == "last"   = last xs
                | str == "rest"   = (LI . tail) xs
                | str == "strip"  = LI . filter (/=(LI [])) $ xs
                | str == "length" = STR . show . length $ xs
            fApply str x
                | str == "rest"   = (LI [])
                | str == "length" = STR "1"
                | otherwise       = x

{--------------------------------------------------------------------
  Templates
--------------------------------------------------------------------}
--change makeTmpl to do notation for clarity?

mkIndex = map ((:) . STR . show . (1+) <*> (:[]) . STR . show)
ix0 = [STR "1",STR "0"]

cycleApp :: (Stringable b) => [([SElem], [SElem]) -> SEnv -> b] -> [([SElem], [SElem])]  -> SEnv -> b
cycleApp x y = mconcatMap (zipWith ($) (cycle x) y) . flip ($)

pluslen xs = zip (map (:[]) xs) $ mkIndex [0..(length xs)]

liTrans :: [SElem] -> [([SElem], [SElem])]
liTrans = pluslen' . paddedTrans SNull . map u
    where u (LI x) = x; u x = [x]
          pluslen' xss@(x:xs) = zip xss $ mkIndex [0..(length x)]

iterApp :: (Stringable a) => [([SElem], [SElem]) -> SEnv -> a] -> [SElem] -> SEnv -> a
iterApp [f] (LI xs:[]) = (mconcatMap $ pluslen xs) . flip f
iterApp [f] vars@(LI xs:vs) = (mconcatMap $ liTrans vars) . flip f
iterApp [f] v = f (v,ix0)
iterApp fs (LI xs:[]) = cycleApp fs (pluslen xs)
iterApp fs vars@(LI xs:vs) = cycleApp fs (liTrans vars)
iterApp fs xs = cycleApp fs (pluslen xs)

anonTmpl :: Stringable a => GenParser Char (Char, Char) (([SElem], [SElem]) -> SEnv -> a)
anonTmpl = around '{' subStmp '}'

regTemplate :: Stringable a => GenParser Char (Char, Char) (([SElem], [SElem]) -> SEnv -> a)
regTemplate = do
  try functn .>> fail "" <|> return ()
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