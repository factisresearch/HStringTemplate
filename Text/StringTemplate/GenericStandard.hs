{-# LANGUAGE FlexibleInstances, OverlappingInstances, UndecidableInstances, Rank2Types, ScopedTypeVariables #-}
--------------------------------------------------------------------
-- | Generic Instance for ToSElem using standard Data.Generic libraries.
--------------------------------------------------------------------}

module Text.StringTemplate.GenericStandard() where
import qualified Data.Map as M
import Text.StringTemplate.Classes
import Text.StringTemplate.Instances()
import Data.Generics.Basics
import Data.Generics.Aliases

gToSElem :: forall a b.(Data a, Stringable b) => a -> SElem b
gToSElem = (\x ->
            case (map stripInitUnder (constrFields . toConstr $ x)) of
              [] -> LI (STR (showConstr (toConstr x)) :
                        (gmapQ gToSElem x))
              fs -> SM (M.fromList (zip fs (gmapQ gToSElem x)))
           )
           `ext1Q` (\t -> case t of (Just x) -> gToSElem x; _ -> SNull)
           `ext1Q` (SM . fmap gToSElem)
           `ext1Q` (LI . map gToSElem)
           `extQ` (toSElem :: Bool -> SElem b)
           `extQ` (toSElem :: Float -> SElem b)
           `extQ` (toSElem :: Double -> SElem b)
           `extQ` (toSElem :: Int -> SElem b)
           `extQ` (toSElem :: Integer -> SElem b)
           `extQ` (toSElem :: String -> SElem b)

instance Data a => ToSElem a
    where toSElem = gToSElem

stripInitUnder :: [Char] -> [Char]
stripInitUnder ('_':s) = stripInitUnder s
stripInitUnder s       = s