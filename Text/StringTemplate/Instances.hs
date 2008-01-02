{-# OPTIONS -O2 -fbang-patterns -fglasgow-exts -fallow-overlapping-instances -fallow-undecidable-instances -optc-O3 #-}

module Text.StringTemplate.Instances where
import Text.StringTemplate.Classes

import qualified Data.Map as M
import Numeric
import Data.Maybe

instance ToSElem String where
    toSElem = STR

instance (ToSElem a) => ToSElem [a] where
    toSElem = LI . map toSElem 

instance (ToSElem a) => ToSElem (M.Map String a) where
    toSElem = SM . fmap toSElem 

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


