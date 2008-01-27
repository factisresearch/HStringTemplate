{-# OPTIONS_GHC -XFlexibleInstances -XOverlappingInstances -XFlexibleContexts -XUndecidableInstances -XRank2Types #-}
--------------------------------------------------------------------
-- | Generic Instance for ToSElem using standard Data.Generic libraries.
--------------------------------------------------------------------}

module Text.StringTemplate.GenericStandard() where
import qualified Data.Map as M
import Text.StringTemplate.Classes
import Text.StringTemplate.Instances()
import Data.Generics.Basics
import Data.Generics.Aliases

gToSElem :: (Data a, Stringable b) => a -> SElem b

gToSElem = (\x ->
            case (map stripInitUnder (constrFields . toConstr $ x)) of
              [] -> LI (STR (dataTypeName (dataTypeOf x)) :
                        (gmapQ gToSElem x))
              fs -> SM (M.fromList (zip fs (gmapQ gToSElem x)))
           )
           `extQ` (toSElem :: Stringable a => Float -> SElem a)
           `extQ` (toSElem :: Stringable a => Double -> SElem a)
           `extQ` (toSElem :: Stringable a => Int -> SElem a)
           `extQ` (toSElem :: Stringable a => Integer -> SElem a)
           `extQ` (toSElem :: Stringable a => String -> SElem a)

instance Data a => ToSElem a
    where toSElem = gToSElem

stripInitUnder :: [Char] -> [Char]
stripInitUnder ('_':s) = stripInitUnder s
stripInitUnder s       = s