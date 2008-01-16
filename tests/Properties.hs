{-# OPTIONS -O2 -fglasgow-exts #-}

module Properties where
import Text.Printf
import Control.Monad
import Control.Arrow hiding (pure)
import Control.Applicative hiding ((<|>),many)
import Data.Maybe
import Data.Monoid
import Data.List
import System.IO
import System.Random hiding (next)
import qualified Data.Map as M

import Text.StringTemplate.Classes
import Text.StringTemplate.Instances
import Text.StringTemplate.Base
import Test.QuickCheck hiding (promote)
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    let n = if null args then 100 else read (head args)
    (results, passed) <- liftM unzip $ mapM
                         (\(s,a) -> printf "%-25s: " s >> a n) tests
    printf "Passed %d tests!\n" (sum passed)
    when (not . and $ results) $ fail "Not all tests passed!"
 where
    tests =
        [("prop_paddedTrans" , mytest prop_paddedTrans),
         ("prop_constStr" , mytest prop_constStr),
         ("prop_emptyNulls" , mytest prop_emptyNulls),
         ("prop_fullNulls" , mytest prop_fullNulls),
         ("prop_substitution" , mytest prop_substitution),
         ("prop_seperator" , mytest prop_seperator),
         ("prop_simpleGroup" , mytest prop_simpleGroup)
        ]

{-----------------------------------------------------------------------
  Limited tests for now: just for list juggling and some basic parsing.
-----------------------------------------------------------------------}

prop_paddedTrans (x::[Int]) (y::[Int]) (z::[Int]) n =
   (length pt == length npt) &&
   all (3 ==) (map length pt) &&
   all (all (==n)) (zipWith unmerge (paddedTrans n pt) [x,y,z])
          where pt   = paddedTrans n [x,y,z]
                npt  = transpose [x,y,z]
                unmerge xl@(x:xs) (y:ys)
                    | x == y = unmerge xs ys
                    | otherwise = xl
                unmerge x y = x

prop_constStr (LitString x) = x == (toString . newSTMP $ x)

prop_emptyNulls (LitString x) (LitString y) i =
    (concat . replicate (abs i) $ x) ==
      (toString . newSTMP . concat . replicate (abs i) $ tmpl)
    where tmpl = x++"$"++y++"$"

prop_fullNulls (LitString x) (LitString y) i =
    length y > 0 ==>
               (concat . replicate (abs i) $ x++y) ==
               (toString . newSTMP . concat . replicate (abs i) $ tmpl)
    where tmpl = x++"$"++y++";null='"++y++"'$"

prop_substitution (LitString x) (LitString y) (LitString z) i =
    length y > 0 ==>
               (concat . replicate (abs i) $ x++z) ==
               (toString . setAttribute y z .
                newSTMP . concat . replicate (abs i) $ tmpl)
    where tmpl = x++"$"++y++"$"

prop_seperator (LitString x) (LitString y) (LitString z) i =
    length x > 0 ==>
               (concat . intersperse z . replicate (abs i) $ y) ==
               (toString . setAttribute x (replicate (abs i) y)
                . newSTMP $ tmpl)
    where tmpl = "$"++x++";seperator='"++z++"'$"

prop_simpleGroup (LitString x) (LitString y) (LitString z) (LitString t) =
    length x > 0 && length y > 0 && length z > 0 && length t > 0 ==>
               x == (toString . fromJust . getStringTemplate x $ grp)
    where tm   = newSTMP x
          tm'  = newSTMP $ "$"++y++"()$"
          tmIt = newSTMP "$it$"
          tm'' = newSTMP $ "$"++z++"():"++t++"()$"
          grp = groupStringTemplates [(y,tm),(z,tm'),(t,tmIt),(x,tm'')]

newtype LitChar = LitChar {unLitChar :: Char} deriving Show
instance Arbitrary LitChar where
  arbitrary = LitChar <$> choose ('a','z')
  coarbitrary = undefined

newtype LitString = LitString String deriving Show
instance Arbitrary LitString where
  arbitrary = LitString . map unLitChar <$> sized (\n -> choose (0,n) >>= vector)
  coarbitrary = undefined
{--------------------------------------------------------------------
  QuickCheck Driver: This lovely code borrowed wholesale from XMonad.
--------------------------------------------------------------------}

mytest :: Testable a => a -> Int -> IO (Bool, Int)
mytest a n = mycheck defaultConfig
    { configMaxTest=n
    , configEvery   = \n args -> let s = show n in s ++ [ '\b' | _ <- s ] } a

mycheck :: Testable a => Config -> a -> IO (Bool, Int)
mycheck config a = do
    rnd <- newStdGen
    mytests config (evaluate a) rnd 0 0 []

mytests :: Config -> Gen Result -> StdGen -> Int -> Int -> [[String]] -> IO (Bool, Int)
mytests config gen rnd0 ntest nfail stamps
    | ntest == configMaxTest config = done "OK," ntest stamps >> return (True, ntest)
    | nfail == configMaxFail config = done "Arguments exhausted after" ntest stamps >> return (True, ntest)
    | otherwise               =
      do putStr (configEvery config ntest (arguments result)) >> hFlush stdout
         case ok result of
           Nothing    ->
             mytests config gen rnd1 ntest (nfail+1) stamps
           Just True  ->
             mytests config gen rnd1 (ntest+1) nfail (stamp result:stamps)
           Just False ->
             putStr ( "Falsifiable after "
                   ++ show ntest
                   ++ " tests:\n"
                   ++ unlines (arguments result)
                    ) >> hFlush stdout >> return (False, ntest)
     where
      result      = generate (configSize config ntest) rnd2 gen
      (rnd1,rnd2) = split rnd0

done :: String -> Int -> [[String]] -> IO ()
done mesg ntest stamps = putStr ( mesg ++ " " ++ show ntest ++ " tests" ++ table )
  where
    table = display . map entry . reverse . sort . map pairLength . group
            . sort . filter (not . null) $ stamps
    display []  = ".\n"
    display [x] = " (" ++ x ++ ").\n"
    display xs  = ".\n" ++ unlines (map (++ ".") xs)
    pairLength xss@(xs:_) = (length xss, xs)
    entry (n, xs)         = percentage n ntest ++ " "
                            ++ concat (intersperse ", " xs)
    percentage n m        = show ((100 * n) `div` m) ++ "%"
