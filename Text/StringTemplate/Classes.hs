{-# OPTIONS -O2 -fglasgow-exts -optc-O3 #-}
module Text.StringTemplate.Classes
    (SElem(..), StringTemplateShows(..), ToSElem(..), SMap, STShow(..),
     First(..)
    ) where
import qualified Data.Map as M
import Data.Monoid


--For 6.6.1 ONLY
newtype First a = First { getFirst :: Maybe a }
	deriving (Eq, Ord, Read, Show)
instance Monoid (First a) where
	mempty = First Nothing
	r@(First (Just _)) `mappend` _ = r
	First Nothing `mappend` r = r


type SMap = M.Map String SElem

data SElem = STR String |STSH STShow | SM SMap | LI [SElem] | SNull deriving (Eq, Ord, Show)

class ToSElem a where
    toSElem :: a -> SElem

class (Eq a, Show a, Ord a) => StringTemplateShows a where
    stringTemplateShow :: a -> String
    stringTemplateFormattedShow :: String -> a -> String
    stringTemplateFormattedShow = flip $ const . stringTemplateShow

data STShow = forall a.(StringTemplateShows a, Show a, Eq a, Ord a) => STShow a 

instance Eq STShow where
    (STShow a) == (STShow b) = show a == show b

instance Ord STShow where
    (STShow a) >= (STShow b) = show a >= show b

instance Show STShow where
    show (STShow a) = "STShow "++show a

