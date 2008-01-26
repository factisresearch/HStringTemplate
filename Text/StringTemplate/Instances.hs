{-# OPTIONS_GHC -XFlexibleInstances #-}
{-# OPTIONS_HADDOCK not-home #-}

module Text.StringTemplate.Instances where
import Text.StringTemplate.Classes

import qualified Data.Map as M
import Numeric
import Data.Ratio
import Data.Array
import Data.Maybe

{--------------------------------------------------------------------
  Additional instances for items that may be set as StringTemplate
  attributes. The code should provide examples of how to proceed.
--------------------------------------------------------------------}
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

instance StringTemplateShows Float where
    stringTemplateShow = flip showFloat ""
    stringTemplateFormattedShow = flip flip [] . showGFloat . fmap fst . listToMaybe . reads

instance ToSElem Float where
    toSElem = stShowsToSE

instance StringTemplateShows Double where
    stringTemplateShow = flip showFloat ""
    stringTemplateFormattedShow = flip flip [] . showGFloat . fmap fst . listToMaybe . reads

instance ToSElem Double where
    toSElem = stShowsToSE

instance ToSElem Int where
    toSElem = STR . show

instance ToSElem Integer where
    toSElem = STR . show

instance Integral a => ToSElem (Ratio a) where
    toSElem = STR . show

instance (ToSElem a, Ix i) => ToSElem (Array i a) where
   toSElem a = toSElem (elems a)

instance (ToSElem a, ToSElem b) => ToSElem (a, b) where
   toSElem (a,b) = LI [toSElem a, toSElem b]
instance (ToSElem a, ToSElem b, ToSElem c) => ToSElem (a, b, c) where
   toSElem (a,b,c) = LI [toSElem a, toSElem b, toSElem c]
instance (ToSElem a, ToSElem b, ToSElem c, ToSElem d) => ToSElem (a, b, c, d) where
   toSElem (a,b,c,d) = LI [toSElem a, toSElem b, toSElem c, toSElem d]
instance (ToSElem a, ToSElem b, ToSElem c, ToSElem d, ToSElem e) => ToSElem (a, b, c, d, e) where
   toSElem (a,b,c,d,e) = LI [toSElem a, toSElem b, toSElem c, toSElem d, toSElem e]