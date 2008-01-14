{-# OPTIONS -O2 -fglasgow-exts #-}
{-# OPTIONS_HADDOCK not-home #-}
module Text.StringTemplate.Classes
    (SElem(..), StringTemplateShows(..), ToSElem(..), SMap, STShow(..),
     StFirst(..), Stringable(..)
    ) where
import qualified Data.Map as M
import Data.List
import Data.Monoid

newtype StFirst a = StFirst { stGetFirst :: Maybe a }
        deriving (Eq, Ord, Read, Show)
instance Monoid (StFirst a) where
        mempty = StFirst Nothing
        r@(StFirst (Just _)) `mappend` _ = r
        StFirst Nothing `mappend` r = r

instance Functor StFirst where
    fmap f x = StFirst . fmap f . stGetFirst $ x

type SMap = M.Map String SElem

data SElem = STR String | STSH STShow | SM SMap | LI [SElem] | SNull deriving (Eq, Ord, Show)

-- | The ToSElem class should be instantiated for all types that can be
-- inserted as attributes into a StringTemplate. Generally new instances
-- should not be defined for atomic types (use 'StringTemplateShows' for that)
-- but rather for containers, most easily by reducing them to maps or lists.
class ToSElem a where
    toSElem :: a -> SElem

-- | The StringTemplateShows class should be instantiated for all types that are
-- directly displayed in a StringTemplate. Its methods all have defaults.
class (Eq a, Show a, Ord a) => StringTemplateShows a where
    -- | Defaults to 'show'.
    stringTemplateShow :: a -> String
    stringTemplateShow = show
    -- | Defaults to  @ flip $ const . stringTemplateShow @
    stringTemplateFormattedShow :: String -> a -> String
    stringTemplateFormattedShow = flip $ const . stringTemplateShow

data STShow = forall a.(StringTemplateShows a, Show a, Eq a, Ord a) => STShow a

instance Eq STShow where
    (STShow a) == (STShow b) = show a == show b

instance Ord STShow where
    (STShow a) >= (STShow b) = show a >= show b

instance Show STShow where
    show (STShow a) = "STShow "++show a

-- | The Stringable class should be instantiated with care.
-- Generally, the provided instances should be enough for anything.
class Monoid a => Stringable a where
    stFromString :: String -> a
    stToString :: a -> String
    -- | Defaults to  @ mconcatMap m k = foldr (mappend . k) mempty m @
    mconcatMap :: [b] -> (b -> a) -> a
    mconcatMap m k = foldr (mappend . k) mempty m
    -- | Defaults to  @ (mconcat .) . intersperse @
    mintercalate :: a -> [a] -> a
    mintercalate = (mconcat .) . intersperse
