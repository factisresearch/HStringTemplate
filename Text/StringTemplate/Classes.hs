{-# LANGUAGE ExistentialQuantification, FlexibleInstances, StandaloneDeriving, GeneralizedNewtypeDeriving, TypeSynonymInstances #-}
{-# OPTIONS_HADDOCK not-home #-}
module Text.StringTemplate.Classes
    (SElem(..), StringTemplateShows(..), ToSElem(..), SMap, STShow(..),
     StFirst(..), Stringable(..), stShowsToSE
    ) where
import qualified Data.Map as M
import Data.List
import Data.Monoid
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Text.PrettyPrint.HughesPJ as PP

newtype StFirst a = StFirst { stGetFirst :: Maybe a }
        deriving (Eq, Ord, Read, Show)
instance Monoid (StFirst a) where
        mempty = StFirst Nothing
        r@(StFirst (Just _)) `mappend` _ = r
        StFirst Nothing `mappend` r = r

instance Functor StFirst where
    fmap f = StFirst . fmap f . stGetFirst

type SMap a = M.Map String (SElem a)

data SElem a = STR String
             | BS LB.ByteString
             | STSH STShow
             | SM (SMap a)
             | LI [SElem a]
             | SBLE a
             | SNull

-- | The ToSElem class should be instantiated for all types that can be
-- inserted as attributes into a StringTemplate.
class ToSElem a where
    toSElem :: Stringable b => a -> SElem b
    toSElemList :: Stringable b => [a] -> SElem b
    toSElemList = LI . map toSElem

-- | The StringTemplateShows class should be instantiated for all types that are
-- directly displayed in a StringTemplate, but take an optional format string. Each such type must have an appropriate ToSElem method defined as well.
class (Show a) => StringTemplateShows a where
    -- | Defaults to 'show'.
    stringTemplateShow :: a -> String
    stringTemplateShow = show
    -- | Defaults to  @ \ _ a -> stringTemplateShow a @
    stringTemplateFormattedShow :: String -> a -> String
    stringTemplateFormattedShow = flip $ const . stringTemplateShow

-- | This method should be used to create ToSElem instances for
-- types defining a custom formatted show function.
stShowsToSE :: (StringTemplateShows a, Stringable b) => a -> SElem b
stShowsToSE = STSH . STShow

data STShow = forall a.(StringTemplateShows a) => STShow a

-- | The Stringable class should be instantiated with care.
-- Generally, the provided instances should be enough for anything.
class Monoid a => Stringable a where
    stFromString :: String -> a
    stFromByteString :: LB.ByteString -> a
    stFromByteString = stFromString . LB.unpack
    stToString :: a -> String
    -- | Defaults to  @ mconcatMap m k = foldr (mappend . k) mempty m @
    mconcatMap :: [b] -> (b -> a) -> a
    mconcatMap m k = foldr (mappend . k) mempty m
    -- | Defaults to  @ (mconcat .) . intersperse @
    mintercalate :: a -> [a] -> a
    mintercalate = (mconcat .) . intersperse
    -- | Defaults to  @  mlabel x y = mconcat [x, stFromString "[", y, stFromString "]"] @
    mlabel :: a -> a -> a
    mlabel x y = mconcat [x, stFromString "[", y, stFromString "]"]

instance Stringable String where
    stFromString = id
    stToString = id

instance Stringable PP.Doc where
    stFromString = PP.text
    stToString = PP.render
    mconcatMap m k = PP.fcat . map k $ m
    mintercalate = (PP.fcat .) . PP.punctuate
    mlabel x y = x PP.$$ PP.nest 1 y

instance Monoid PP.Doc where
    mempty = PP.empty
    x `mappend` y = x PP.<> y

instance Stringable B.ByteString where
    stFromString = B.pack
    stFromByteString = B.concat . LB.toChunks
    stToString = B.unpack

instance Stringable LB.ByteString where
    stFromString = LB.pack
    stFromByteString = id
    stToString = LB.unpack

instance Stringable (Endo String) where
    stFromString = Endo . (++)
    stToString = ($ []) . appEndo
