{-# LANGUAGE FlexibleInstances, OverlappingInstances, FlexibleContexts, UndecidableInstances, Rank2Types #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
--------------------------------------------------------------------
-- | Generic Instance for ToSElem using syb-with-class.
--   Inspired heavily-to-entirely by Alex Drummond's RJson.
--------------------------------------------------------------------}

module Text.StringTemplate.GenericWithClass() where
import qualified Data.Map as M
import Text.StringTemplate.Classes
import Data.Generics.SYB.WithClass.Basics

stripInitialUnderscores :: String -> String
stripInitialUnderscores ('_':s) = stripInitialUnderscores s
stripInitialUnderscores s       = s

data ToSElemD a = ToSElemD { toSElemD :: Stringable b => a -> SElem b }

toSElemProxy :: Proxy ToSElemD
toSElemProxy = error "This value should never be evaluated!"

instance (ToSElem a, Data ToSElemD a) => Sat (ToSElemD a) where
   dict = ToSElemD { toSElemD = toSElem }

genericToSElem :: (Data ToSElemD a, ToSElem a, Stringable b) => a -> SElem b
genericToSElem x
       | isAlgType (dataTypeOf toSElemProxy x) =
           case (map stripInitialUnderscores (getFields x)) of
             [] -> LI (STR (showConstr (toConstr toSElemProxy x)) :
                           gmapQ toSElemProxy (toSElemD dict) x)
             fs -> SM (M.fromList (zip fs (gmapQ toSElemProxy (toSElemD dict) x)))
       | True =
               error ("Unable to serialize the primitive type '" ++
                      dataTypeName (dataTypeOf toSElemProxy x) ++ "'")

getFields :: Data ToSElemD a => a -> [String]
getFields = constrFields . toConstr toSElemProxy

instance Data ToSElemD t => ToSElem t where
   toSElem = genericToSElem
