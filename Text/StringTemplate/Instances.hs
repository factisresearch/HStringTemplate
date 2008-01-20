{-# OPTIONS -fglasgow-exts -fallow-overlapping-instances -fallow-undecidable-instances #-}
{-# OPTIONS_HADDOCK not-home #-}

module Text.StringTemplate.Instances where
import Text.StringTemplate.Classes

import qualified Data.Map as M
import Numeric
import Data.Maybe

{--------------------------------------------------------------------
  Additional instances for items that may be set as StringTemplate
  attributes. The code should provide examples of how to proceed.
--------------------------------------------------------------------}
--Todo: provide instances of ToSElem for all Folddable
--Todo: provide instances of StringTemplateShows for dates
--      and other common cases.

instance ToSElem Char where
    toSElem = STR . (:[])
    toSElemList = STR
    
instance ToSElem Bool where
    toSElem True = STR ""
    toSElem _ = SNull

instance (ToSElem a) => ToSElem [a] where
    toSElem = toSElemList

instance (ToSElem a) => ToSElem (M.Map String a) where
    toSElem = SM . fmap toSElem

instance (StringTemplateShows a) => ToSElem a where
    toSElem = STSH . STShow

instance (Show a) => StringTemplateShows a where
    stringTemplateShow a = show a

instance StringTemplateShows Float where
    stringTemplateShow = flip showFloat ""
    stringTemplateFormattedShow = flip flip [] . showGFloat . fmap fst . listToMaybe . reads

instance StringTemplateShows Double where
    stringTemplateShow = flip showFloat ""
    stringTemplateFormattedShow = flip flip [] . showGFloat . fmap fst . listToMaybe . reads
