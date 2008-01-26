{-# OPTIONS_HADDOCK not-home #-}

module Text.StringTemplate.Group
    (groupStringTemplates, addSuperGroup, addSubGroup, setEncoderGroup,
     mergeSTGroups, directoryGroup, optInsertGroup, unsafeVolatileDirectoryGroup
    ) where
import Control.Applicative hiding ((<|>),many)
import Control.Arrow
import Data.Monoid
import Data.List
import System.Time
import System.FilePath
import System.Directory
import Data.IORef
import System.IO.Unsafe
import System.IO.Error
import qualified Data.Map as M

import Text.StringTemplate.Base
import Text.StringTemplate.Classes

{--------------------------------------------------------------------
  Utilities
--------------------------------------------------------------------}

(<$$>) :: (Functor f1, Functor f) => (a -> b) -> f (f1 a) -> f (f1 b)
(<$$>) x y = ((<$>) . (<$>)) x y

sgInsert :: (String -> StFirst (StringTemplate a)) -> StringTemplate a -> StringTemplate a
sgInsert   g st = let e = senv st in st {senv = e {sgen = sgen e `mappend` g} }
sgOverride :: STGroup a -> StringTemplate a -> StringTemplate a
sgOverride g st = let e = senv st in st {senv = e {sgen = g `mappend` sgen e} }


{--------------------------------------------------------------------
  Group API
--------------------------------------------------------------------}

-- | Given a list of named of StringTemplates, returns a group which generates
-- them such that they can call one another.
groupStringTemplates :: [(String,StringTemplate a)] -> STGroup a
groupStringTemplates xs = newGen
    where newGen s = StFirst (M.lookup s ng)
          ng = foldl' (flip $ uncurry M.insert) M.empty $
               map (second $ sgInsert newGen) xs

-- | Given a path, returns a group which generates all files in said directory
-- which have the proper \"st\" extension.
-- This function is strict, with all files read once. As it performs file IO,
-- expect it to throw the usual exceptions.
directoryGroup :: (Stringable a) => FilePath -> IO (STGroup a)
directoryGroup path = groupStringTemplates <$>
                      (fmap <$> zip . (map dropExtension)
                       <*> mapM (newSTMP <$$> readFile)
                           =<< filter (("st" ==) . takeExtension)
                       <$> getDirectoryContents path)

-- | Adds a supergroup to any StringTemplate group such that templates from
-- the original group are now able to call ones from the supergroup as well.
addSuperGroup :: STGroup a -> STGroup a -> STGroup a
addSuperGroup f g = sgInsert g <$$> f

-- | Adds a \"subgroup\" to any StringTemplate group such that templates from
-- the original group now have template calls \"shadowed\" by the subgroup.
addSubGroup :: STGroup a -> STGroup a -> STGroup a
addSubGroup f g = sgOverride g <$$> f

-- | Merges two groups into a single group. This function is left-biased,
-- prefering bindings from the first group when there is a conflict.
mergeSTGroups :: STGroup a -> STGroup a -> STGroup a
mergeSTGroups f g = addSuperGroup f g `mappend` addSubGroup g f

-- | Adds a set of global options to a group
optInsertGroup :: [(String, String)] -> STGroup a -> STGroup a
optInsertGroup opts f = optInsertTmpl opts <$$> f

-- | Sets an encoding function of a group that all values are
-- rendered with in each enclosed template
setEncoderGroup :: (Stringable a) => (String -> String) ->  STGroup a -> STGroup a
setEncoderGroup x f = setEncoder x <$$> f

{-# NOINLINE unsafeVolatileDirectoryGroup #-}

-- | Given an integral amount of seconds and a path, returns a group generating
-- all files in said directory with the proper \"st\" extension,
-- cached for that amount of seconds. IO errors are \"swallowed\" by this so
-- that exceptions don't arise in unexpected places.
-- This violates referential transparency, but can be very useful in developing
-- templates for any sort of server application.
-- It should be swapped out for production purposes.
unsafeVolatileDirectoryGroup :: Stringable a => String -> Int -> IO (STGroup a)
unsafeVolatileDirectoryGroup path i = return (cacheSTGroup i stfg)
    where stfg = StFirst . Just . STMP (SEnv M.empty [] stfg id)
                 . parseSTMP ('$', '$') . unsafePerformIO . flip catch
                       (return . ("IO Error: " ++) . ioeGetErrorString)
                 . readFile . (path </>)
          cacheSTGroup :: Int -> STGroup a -> STGroup a
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
                        where udReturn now = do
                                let st = g s
                                atomicModifyIORef r $
                                  flip (,) () . M.insert s (now, st)
                                return st