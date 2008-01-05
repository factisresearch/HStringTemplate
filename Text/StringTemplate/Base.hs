{-# OPTIONS -O2 -fbang-patterns -fglasgow-exts -optc-O3 #-}

module Text.StringTemplate.Base
    (StringTemplate, toString, newSTMP, newAngleSTMP, 
     StringTemplateShows(..), ToSElem(..), STGen, setAttribute,
     groupStringTemplates, addSuperGroup, addSubGroup, mergeSTGroups,
     stringTemplateFileGroup, cacheSTGroup
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

import Text.StringTemplate.Classes
import Debug.Trace --DEBUG

{--------------------------------------------------------------------
  Utilities and Types
--------------------------------------------------------------------}

instance Applicative (GenParser tok st) where pure = return; (<*>) = ap;

o = (.).(.)
(|.) f g x = f (g x)
(.>>) f g = f >> g
infixr 5 .>>
infixr 3 |.
infixr 8 `o`
intercalate = concat `o` intersperse
swing = flip . (. flip id)
paddedTrans n xs = take lx $ trans' xs
    where lx = maximum (map length xs)
          trans' [] = []
          trans' ([]:xss)  = trans' xss
          trans' ((x:xs) : xss) = (x : map h xss) : trans' (m xs:(map t xss))
          h (x:xs) = x; h _ = n; t (x:y:xs) = (y:xs); t _ = [n];
          m (x:xs) = (x:xs); m _ = [n];

{--------------------------------------------------------------------
  StringTemplate and the API
--------------------------------------------------------------------}
--add noInline to STFG

-- | A Function that generates StringTemplates
type STGen = String -> (First StringTemplate)

-- | A String with \"holes\" in it.
data StringTemplate = STMP {senv :: SEnv, runSTMP :: (SEnv -> String)}

-- | Renders a StringTemplate to a String.
toString :: StringTemplate -> String
toString = runSTMP <*> senv

-- | Parses a String to produce a StringTemplate, with '$'s as delimiters.
-- It is constructed with a stub group that cannot look up other templates.
newSTMP :: String -> StringTemplate
newSTMP = STMP (SEnv M.empty [] mempty) . parseSTMP ('$','$')

-- | Parses a String to produce a StringTemplate, delimited by '<' and '>'.
-- It is constructed with a stub group that cannot look up other templates.
newAngleSTMP :: String -> StringTemplate
newAngleSTMP = STMP (SEnv M.empty [] mempty) . parseSTMP ('<','>')

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

-- | Adds a supergroup to any StringTemplate group such that templates from
-- the original group are now able to call ones from the supergroup as well.
addSuperGroup :: STGen -> STGen -> STGen
addSuperGroup f g = First . maybe Nothing (Just . sgInsert g) . getFirst . f

-- | Adds a "subgroup" to any StringTemplate group such that templates from
-- the original group now have template calls "shadowed" by the subgroup.
addSubGroup :: STGen -> STGen -> STGen
addSubGroup f g = First . maybe Nothing (Just . sgOverride g) . getFirst . f

-- | Merges two groups into a single group. This function is left-biased,
-- prefering bindings from the first group when there is a conflict.
mergeSTGroups :: STGen -> STGen -> STGen
mergeSTGroups f g = addSuperGroup f g `mappend` addSubGroup g f

-- | Given a path, returns a group which generates all files in said directory.
stringTemplateFileGroup :: String -> STGen
stringTemplateFileGroup path = stfg
  where stfg =  First . Just . STMP (SEnv M.empty [] stfg) .
                parseSTMP ('$','$') . unsafePerformIO . readFile . (path </>)

-- | Given an integral amount of seconds and a group, returns a group cached for
-- that span of time.
cacheSTGroup :: Int -> STGen -> STGen
cacheSTGroup m g = unsafePerformIO $ go <$> newIORef M.empty
    where go r s = unsafePerformIO $ do
                     mp <- readIORef r
                     now <- getClockTime
                     maybe (udReturn mp now)
                      (\(t, st) -> if (tdSec . normalizeTimeDiff $
                           diffClockTimes now t) > m
                         then udReturn mp now
                         else return $ First . Just $ st)
                      . M.lookup s $ mp
              where udReturn mp now = maybe (return $ First Nothing)
                                      (\st -> do
                                         atomicModifyIORef r $
                                          flip (,) () . M.insert s (now, st)
                                         return $ First . Just $ st)
                                      . getFirst . g $ s

{--------------------------------------------------------------------
  Internal API
--------------------------------------------------------------------}
--TODO we ignore wrap, anchor and format options, as well as indentation
--Switch to using ShowS for performance?

data SEnv = SEnv {smp :: SMap, sopts :: [(String, SEnv -> SElem)], sgen :: STGen}

envLookup x = M.lookup x . smp
envInsert (s, x) y = y {smp = M.insert s x (smp y)}
envInsApp  s  x  y = y {smp = M.insertWith app s x (smp y)}
    where app x (LI ys) = LI (x:ys)
          app x y = LI [x,y]

optLookup x = lookup x . sopts
optInsert x env = env {sopts = x ++ sopts env}
nullOpt = fromMaybe (justSTR "") =<< optLookup "null"

stLookup x env = maybe (newSTMP ("No Template Found for: " ++ x))
                 (\st-> st {senv = env}) $ getFirst (sgen env x)

sgInsert   g st = let e = senv st in st {senv = e {sgen = sgen e `mappend` g} }
sgOverride g st = let e = senv st in st {senv = e {sgen = g `mappend` sgen e} }

showVal :: SEnv -> SElem -> String
showVal snv se =
    case se of (STR x)-> x; (LI xs)-> joinUp xs; (SM sm)-> joinUp $ M.elems sm
               (STSH x)-> format x; SNull -> showVal <*> nullOpt $ snv 
    where sepVal = fromMaybe (justSTR "") =<< optLookup "seperator" $ snv
          format = maybe stshow . stfshow <*> optLookup "format" $ snv
          joinUp = intercalate (showVal snv sepVal) . map (showVal snv)

parseSTMP x = either (const . show) id . runParser stmpl x "in"

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
props = many $ char '.' >> (around '(' exprn ')' <|> justSTR <$> word)

escapedChar chs =
    noneOf chs >>= \x -> if x == '\\' then anyChar >>= \y ->
    if y `elem` chs then return [y] else return [x, y] else return [x]
escapedStr chs = concat <$> many1 (escapedChar chs)

{--------------------------------------------------------------------
  The Grammar
--------------------------------------------------------------------}

stmpl :: GenParser Char (Char,Char) (SEnv -> String)
stmpl= do
  (ca, cb) <- getState
  mconcat <$> many1 (const <$> escapedStr (ca:[]) <|>
                     try (around ca optExpr cb) <|> try comment <?> "template")

subStmp = do
  (ca, cb) <-getState
  udEnv <-option (transform ["it"])
           (transform <$> try attribNames)
  st <-mconcat <$> many (const <$> escapedStr (ca:"}|") <|> around ca optExpr cb
                          <|> try comment <?> "subtemplate")
  return (st `o` udEnv)
      where transform an (att,i:i0:[]) = 
                flip (foldr envInsert) $ zip ("i":"i0":an) (i:i0:att)
            attribNames = (char '|' >>) . return =<< comlist (spaced word)

comment = do
  (ca, cb) <- getState
  string (ca:'!':[]) >> manyTill anyChar (try . string $ '!':cb:[])
  return (const "")

optExpr = do
  (ca, cb) <- getState
  (try (string ("else"++[cb])) <|> try (string "elseif(") <|>
    try (string "endif")) .>> fail "Malformed If Statement." <|> return ()
  (expr,opts) <- liftM2 (,) (spaced exprn) (many opt)
  skipMany (char ';')
  return ((showVal <*> expr) . optInsert opts)
      where opt = around ';' (spaced word) '=' >>= (<$> spaced exprn) . (,)

{--------------------------------------------------------------------
  Statements
--------------------------------------------------------------------}

getProp (p:ps) (SM mp) = maybe <$> const SNull <*> flip (getProp ps)
                         <*> (flip M.lookup mp |. showVal <*> p)
getProp (p:ps) _ = const SNull
getProp _ se = const se

ifIsSet t e n SNull = if n then e else t
ifIsSet t e n _ = if n then t else e

substat = try elseifstat <|> try elsestat <|> endifstat

parseif cb = (,,,,,) <$> option True (char '!' >> return False) <*> exprn <*>
           props <*> char ')' .>> char cb <*> stmpl <*> substat

stat = do
  (ca, cb) <- getState
  string "if("
  (n, e, p, _, act, cont) <- parseif cb
  return (STR `o` ifIsSet act cont n =<< getProp p =<< e)

elseifstat = do
  (ca, cb) <- getState
  char ca >> string "elseif("
  (n, e, p, _, act, cont) <- parseif cb
  return (ifIsSet act cont n =<< getProp p =<< e)

elsestat = do
  (ca, cb) <- getState
  around ca (string "else") cb
  act <- stmpl
  char ca >> string "endif"
  return act

endifstat = getState >>= char . fst >> string "endif" >> return (const "")

{--------------------------------------------------------------------
  Expressions
--------------------------------------------------------------------}

exprn :: GenParser Char (Char,Char) (SEnv -> SElem)
exprn = do
  exprs <- (:[]) <$> try stat <|> comlist subexprn <?> "expression"
  templ <- option (const . head)
           (char ':' >> iterApp <$> comlist (anonTmpl <|> regTemplate))
  return (templ =<< sequence exprs)

subexprn = cct <$> spaced
            (braceConcat <|> STR `o` ($ ([SNull],ix0)) <$> try regTemplate <|>
             attrib <|> STR `o` ($ ([SNull],ix0)) <$> anonTmpl <?> "expression")
           `sepBy1` spaced (char '+')
    where cct xs@(x:y:z) = STR . (concatMap <$> showVal <*> sequence xs)
          cct (x:xs) = x
 
braceConcat = LI . foldr go [] `o` sequence <$> around '['(comlist attrib)']'
    where go (LI x) lst = x++lst; go x lst = x:lst

literal = justSTR <$> (around '"' (concat <$> many (escapedChar "\"")) '"'
                       <|> around '\'' (concat <$> many (escapedChar "'")) '\'')

attrib = do
  a <- literal <|> try functn <|> prepExp <$> word <|> around '(' exprn ')'
         <?> "attribute"
  proprs <- props
  return (getProp proprs =<< a)
      where prepExp var = fromMaybe SNull <$> envLookup var

functn = do
  f <- string "first" <|> string "rest" <|> string "strip"
       <|> try (string "length") <|> string "last"
  (fApply f .) <$> around '(' exprn ')'
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

mkIndex = map ((:) . STR . show . (1+) <*> (:[]) . STR . show)
ix0 = [STR "1",STR "0"]

cycleApp = mconcat `o` zipWith ($) . cycle

pluslen xs = zip (map (:[]) xs) $ mkIndex [0..(length xs)]

liTrans = pluslen' . paddedTrans SNull . map u
    where u (LI x) = x; u x = [x]
          pluslen' xss@(x:xs) = zip xss $ mkIndex [0..(length x)]

iterApp (f:[]) (LI xs:[]) = STR . (pluslen xs >>=) . flip f
iterApp (f:[]) vars@(LI xs:vs) = STR . (liTrans vars >>=) . flip f
iterApp (f:[]) v = STR . f (v,ix0)

iterApp fs (LI xs:[]) = STR . cycleApp fs (pluslen xs)
iterApp fs vars@(LI xs:vs) = STR . cycleApp fs (liTrans vars)
iterApp fs xs = STR . cycleApp fs (pluslen xs)

anonTmpl = around '{' subStmp '}'

regTemplate = do
  try functn .>> fail "" <|> return ()
  name <- justSTR <$> word <|> around '(' exprn ')'
  vals <- around '(' (spaced $ try assgn <|> anonassgn <|> return []) ')'
  return (join . (. name) . makeTmpl vals)
      where makeTmpl v ((se:_),is) (STR x) = 
                toString |. stBind . (zip ["it","i","i0"] (se:is) ++)
                            . swing (map . second) v <*> stLookup x
            makeTmpl _ _ _ = const "Invalid Template Specified"
            stBind v st = st {senv = foldr envInsert (senv st) v}
            anonassgn = (:[]) . (,) "it" <$> exprn
            assgn = (spaced word >>= (<$> char '=' .>> spaced exprn) . (,))
                    `sepEndBy1` char ';'

--DEBUG
rP p str = either (const . STR . show) id (parse p "input" str)
tsM = M.insert "foo" (LI [STR "f1"]) (M.singleton "bar" (LI [STR "barr",STR "baz"]))

pTrace s = try $
         do
           x <- try $ many1 anyChar
           trace (s++": " ++x) $ try $ char 'z'
           fail x
