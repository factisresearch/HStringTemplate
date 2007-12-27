{-# OPTIONS -O2 -fbang-patterns -fglasgow-exts -fallow-overlapping-instances -fallow-undecidable-instances -optc-O3 #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Text.StringTemplate.Base
-- Copyright   :  (c) Sterling Clover 2008
-- License     :  BSD-style
-- Maintainer  :  s.clover@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-- Version     :  0.1
--
-- A StringTemplate is a String with \"holes\" in it.
-- This is a port of the Java StringTemplate library written by Terrence Parr.
-- (<http://www.stringtemplate.org>).
--
-- This library implements the basic 3.0 grammar, lacking Groups,
-- Regions, and Interfaces.
-- Additionally, it does not yet include conditionals, the application of
-- multivalued templates to alternating attributes, or automated tabs and
-- wrapping.
-----------------------------------------------------------------------------

module Text.StringTemplate.Base
    (StringTemplate, toString, newStubSTMP, newAngleStubSTMP, SElem
    ) where
import Control.Monad
import Control.Arrow hiding (pure)
import Control.Applicative hiding ((<|>),many)
import Data.Maybe
import Data.Monoid
import Data.List
import Numeric
import Text.ParserCombinators.Parsec
import qualified Data.Map as M

import Debug.Trace --DEBUG

{--------------------------------------------------------------------
  Utilities and Types
--------------------------------------------------------------------}

instance Applicative (GenParser tok st) where pure = return; (<*>) = ap;

o = (.).(.)
intercalate = concat `o` intersperse
paddedTrans n xs = take lx $ trans' xs
    where lx = maximum (map length xs)
          trans' [] = []
          trans' ([]:xss)  = trans' xss
          trans' ((x:xs) : xss) = (x : map h xss) : trans' (m xs:(map t xss))
          h (x:xs) = x; h _ = n; t (x:y:xs) = (y:xs); t _ = [n];
          m (x:xs) = (x:xs); m _ = [n];

type SMap = M.Map String SElem
data SElem = STR String |STSH STShow | SM SMap | LI [SElem] | SNull deriving (Eq, Ord, Show)
justSTR = const . STR

{--------------------------------------------------------------------
  StringTemplate and the API
--------------------------------------------------------------------}
--TODO: add lookup and addition functions to it. Move the fancy stuff to elsewhere.

-- | A String with \"holes\" in it.
data StringTemplate = STMP {stenv :: SEnv, runSTMP :: (SEnv -> String)}

-- | Renders a StringTemplate to a String.
toString :: StringTemplate -> String
toString = runSTMP <*> stenv

-- | Parses a String to produce a StringTemplate, with '$'s as delimiters.
-- It is constructed with a stub group that cannot look up other templates.
newStubSTMP :: String -> StringTemplate
newStubSTMP = STMP (SEnv M.empty [] stubSGen) . parseSTMP ('$','$')

-- | Parses a String to produce a StringTemplate with '<' and '>' as delimiters.
-- It is constructed with a stub group that cannot look up other templates.
newAngleStubSTMP :: String -> StringTemplate
newAngleStubSTMP = STMP (SEnv M.empty [] stubSGen) . parseSTMP ('<','>')

class ToSElem a where
    toSElem :: a -> SElem

instance ToSElem String where
    toSElem = STR

instance (ToSElem a) => ToSElem [a] where
    toSElem = LI . map toSElem 

instance (ToSElem a) => ToSElem (M.Map String a) where
    toSElem = SM . fmap toSElem 

class (Eq a, Show a, Ord a) => StringTemplateShows a where
    stringTemplateShow :: a -> String
    stringTemplateFormattedShow :: String -> a -> String
    stringTemplateFormattedShow = flip $ const . stringTemplateShow

data STShow = forall a.(StringTemplateShows a, Show a, Eq a, Ord a) => STShow a 
stshow (STShow a) = stringTemplateShow a
stfshow f (STShow a) = stringTemplateFormattedShow f a

instance Eq STShow where
    (STShow a) == (STShow b) = show a == show b

instance Ord STShow where
    (STShow a) >= (STShow b) = show a >= show b

instance Show STShow where
    show (STShow a) = "STShow "++show a

instance (StringTemplateShows a) => ToSElem a where
    toSElem = STSH . STShow

instance (Show a, Eq a, Ord a) => StringTemplateShows a where
    stringTemplateShow a = show a

instance StringTemplateShows Float where
    stringTemplateShow = flip showFloat ""
    stringTemplateFormattedShow = flip flip [] . showGFloat . fmap fst . listToMaybe . reads

instance StringTemplateShows Double where
    stringTemplateShow = flip showFloat ""
    stringTemplateFormattedShow = flip flip [] . showGFloat . fmap fst . listToMaybe . reads

setAttribute :: (ToSElem a) => String -> a -> StringTemplate -> StringTemplate
setAttribute s x st = st {stenv = envInsApp s (toSElem x) (stenv st)}


{--------------------------------------------------------------------
  Internal API
--------------------------------------------------------------------}
--TODO we ignore wrap, anchor and format options, as well as indentation
--Switch to using ShowS

data SEnv = SEnv {senv :: SMap, sopts :: [(String, SEnv -> SElem)], sgen :: String -> StringTemplate}

stubSGen s = newStubSTMP ("No Template Found for: " ++ s)

envLookup x = M.lookup x . senv
optLookup x = lookup x . sopts
nullOpt = fromMaybe (justSTR "") =<< optLookup "null"
stLookup x env = (sgen env x) {stenv = env}

envInsert s x y = y {senv = M.insert s x (senv y)}
envInsApp s x y = y {senv = M.insertWith app s x (senv y)}
    where app x (LI ys) = LI (x:ys)
          app x y = LI [x,y]

optInsert x env = env {sopts = x ++ sopts env}

stBind :: [(String, SEnv -> SElem)] -> StringTemplate -> StringTemplate
stBind v st = st {stenv = foldr (uncurry ((=<<) . envInsert)) (stenv st) v}

showVal :: SEnv -> SElem -> String
showVal snv se =
    case se of (STR x)-> x; (LI xs)-> joinUp xs; (SM sm)-> joinUp $ M.elems sm
               (STSH x)-> stshow x; SNull -> showVal <*> nullOpt $ snv 
    where sepVal  = fromMaybe (justSTR "") =<< optLookup "seperator" $ snv
          joinUp  = intercalate (showVal snv sepVal) . map (showVal snv)

parseSTMP (ca,cb) = mconcat . either ((:[]) . const . show) id
                    . runParser stmpl (ca,cb) "in"

{--------------------------------------------------------------------
  Utility Combinators
--------------------------------------------------------------------}

around x p y = do {char x; v<-p; char y; return v}
spaced p = do {spaces; v<-p; spaces; return v}
word = many1 alphaNum
comlist p = spaced (p `sepBy1` spaced (char ','))

escapedChar chs =
    noneOf chs >>= \x -> if x == '\\' then anyChar >>= \y ->
    if y `elem` chs then return [y] else return [x, y] else return [x]
escapedStr chs = concat <$> many1 (escapedChar chs)

{--------------------------------------------------------------------
  The Grammar
--------------------------------------------------------------------}
-- TODO if then else as STATEMENTS
-- alternt templates to multival attributes i.e.: $names:blueItem(),greenItem()$
-- Comments

stmpl :: GenParser Char (Char,Char) [SEnv -> String]
stmpl= do
  (ca, cb) <- getState
  many (const <$> escapedStr (ca:[]) <|> around ca optExpr cb <?> "template")

subStmp = do
  (ca, cb) <- getState
  udEnv <- option (transform ["it","i","i0"])
           (transform . (++["i","i0"]) <$> try attribNames)
  st <- mconcat <$> many (const <$> escapedStr (ca:"}")
                          <|> around ca optExpr cb <?> "subtemplate")
  return (st `o` udEnv)
      where transform  = flip (foldr (uncurry envInsert)) `o` zip
            attribNames = do
              w <- comlist (spaced word)
              char '|'
              return w

optExpr = do
  expr <- spaced exprn
  opts <- many opt 
  skipMany (char ';')
  return ((showVal <*> expr) . optInsert opts)
      where opt = do
              var <- around ';' (spaced word) '='
              expr <- spaced exprn
              return (var, expr)

{--------------------------------------------------------------------
  Expressions
--------------------------------------------------------------------}

exprn :: GenParser Char (Char,Char) (SEnv -> SElem)
exprn = do
  (ca, cb) <- getState
  ((try (string ("else"++[cb]) <|> string "if") <|> try (string "endif")) >> fail "") <|> return ()
  exprs <- (:[]) <$> try stat <|> comlist subexprn <?> "expression"
  templ <- option (const . head) (char ':' >> (anonTmpl <|> regTemplate))
  return (templ =<< sequence exprs)

stat = do
  string "if("
  n <- option id (char '!' >> return not)
  e <- exprn
  p <- many (char '.' >> (around '(' exprn ')' <|> justSTR <$> word))
  char ')'
  (ca, cb) <- getState
  char cb
  sst <- substat
  return (STR . (toStat sst n =<< getProp p =<< e))
      where toStat sst n e = sst (n e)
            getProp (p:ps) (SM mp) = maybe <$> const False <*> flip (getProp ps)
                                     <*> (flip M.lookup mp . (showVal <*> p))
            getProp (p:ps) _ = const False
            getProp _ se = const True
--alsoelse, endif, factor cleanly

substat = do
  stuff <- mconcat <$> stmpl
  string "elseif("
  n <- option id (char '!' >> return not)
  e <- exprn
  p <- many (char '.' >> (around '(' exprn ')' <|> justSTR <$> word))
  char ')'
  (ca, cb) <- getState
  char cb
  sst <- substat
  return (\pred -> (if pred then stuff else const "") `mappend` (toStat sst n =<< getProp p =<< e))
      where toStat sst n e = sst (n e)
            getProp (p:ps) (SM mp) = maybe <$> const False <*> flip (getProp ps)
                                     <*> (flip M.lookup mp . (showVal <*> p))
            getProp (p:ps) _ = const False
            getProp _ se = const True


subexprn = cct <$> ((`sepBy1` spaced (char '+')) $ spaced (braceConcat <|>
                     ($ [STR ""]) <$> try regTemplate <|> attrib <|>
                     ($ [STR ""]) <$> anonTmpl <?> "expression"))
    where cct xs@(x:y:z) = STR . (concatMap <$> showVal <*> sequence xs)
          cct (x:xs) = x
 
braceConcat = (LI . foldr go []) `o` sequence <$> around '['(comlist attrib)']'
    where go (LI x) lst = x++lst; go x lst = x:lst

literal = justSTR <$> (around '"' (concat <$> many (escapedChar "\"")) '"'
                       <|> around '\'' (concat <$> many (escapedChar "'")) '\'')

attrib = do
  a <- literal <|> try functn <|> prepExp <$> word <|> around '(' exprn ')'
         <?> "attribute"
  props <- many (char '.' >> (around '(' exprn ')' <|> justSTR <$> word))
  return (getProp props =<< a)
      where prepExp var = fromMaybe <$> nullOpt <*> (envLookup var)
            getProp (p:ps) (SM mp) = maybe <$> nullOpt <*> (flip (getProp ps)) 
                                     <*> (flip M.lookup mp . (showVal <*> p))
            getProp (p:ps) _ = nullOpt
            getProp _ se = const se

functn = do
  f <- string "first" <|> string "rest" <|> string "strip"
       <|> try (string "length") <|> string "last"
  e <- around '(' exprn ')'
  return (fApply f . e)
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

anonTmpl = mkAnon <$> around '{' subStmp '}'
    where
      mkAnon stmp ((LI xs):[]) = STR . (pluslen xs >>=) . flip stmp
      mkAnon stmp vars@((LI xs):vs) =  STR . (liTrans vars >>=) . flip stmp
      mkAnon stmp v =  STR . stmp v
      liTrans = pluslen' . paddedTrans SNull . map u
          where u (LI x) = x; u x = [x]
      pluslen' xss@(x:xs) = zipWith (++) xss $ mkIndex [0..(length x)]
      pluslen xs = zipWith (:) xs $ mkIndex [0..(length xs)]
      mkIndex = map (\x -> [STR (show (x+1)), STR (show x)])

regTemplate = do
  (try functn >> fail "") <|> return ()
  name <- justSTR <$> word <|> around '(' exprn ')'
  vals <- around '(' (spaced $ try assgn <|> anonassgn <|> return []) ')'
  return (\se env -> (makeTmpl vals (name env) se env))
      where makeTmpl v (STR x) (se:ses) =
                STR . toString . stBind (("it",const se):v) . stLookup x
            makeTmpl _ _ _ = justSTR "Invalid Template Specified"
            anonassgn = ((:[]) . (,) "it" <$> exprn)
            assgn = (`sepEndBy1` char ';') $ do
                v <- spaced word
                char '='
                e <- spaced exprn
                return (v, e)

--DEBUG
rP p str = either ( const . STR . show) id (parse p "input" str)
tsM = M.insert "foo" ((LI [STR "f1"])) (M.singleton "bar" (LI [STR "barr",STR "baz"]))