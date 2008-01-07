{-# OPTIONS -O2 -fbang-patterns -fglasgow-exts -optc-O3 #-}

--QuickCheck driver is lifted wholesale from XMonad

module Text.StringTemplate.Test where
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

main = do
    args <- getArgs
    let n = if null args then 100 else read (head args)
    (results, passed) <- liftM unzip $ mapM
                         (\(s,a) -> printf "%-25s: " s >> a n) tests
    printf "Passed %d tests!\n" (sum passed)
    when (not . and $ results) $ fail "Not all tests passed!"
 where
    tests =
        [("prop_constStr" , mytest prop_constStr),
         ("prop_emptyNulls" , mytest prop_emptyNulls),
         ("prop_fullNulls" , mytest prop_fullNulls)
        ]

prop_constStr x = x' == (toString . newSTMP $ x')
    where x' = map unLitChar x

prop_emptyNulls x y (i::Int) = (concat . replicate i $ x') ==
                            (toString . newSTMP . concat . replicate i $ z)
    where x' = map unLitChar x
          y' = map unLitChar y
          z = x'++"$"++y'++"$"

prop_fullNulls x y (i::Int) = length y > 0 ==>
                             (concat . replicate i $ x'++y') ==
                             (toString . newSTMP . concat . replicate i $ z)
    where x' = map unLitChar x
          y' = map unLitChar y
          z = x'++"$"++y'++";null='"++y'++"'$"

newtype LitChar = LitChar Char deriving Show
unLitChar (LitChar x) = x
instance Arbitrary LitChar where
  arbitrary = do
      x <- choose ('a','z')
      return $ LitChar x


{--------------------------------------------------------------------
  QuickCheck Driver: This freaking awesome code borrowed wholesale
    from XMonad.
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