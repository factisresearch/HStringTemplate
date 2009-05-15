{-# LANGUAGE DeriveDataTypeable, QuasiQuotes #-}
module Text.StringTemplate.QQ (stmp) where

import Data.Generics
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote
import Text.StringTemplate.Base
import Text.ParserCombinators.Parsec
import Control.Monad.Writer
import Control.Applicative

quoteTmplExp :: String -> TH.ExpQ
quoteTmplPat :: String -> TH.PatQ

stmp = QuasiQuoter quoteTmplExp quoteTmplPat

quoteTmplPat = error "Cannot apply stmp quasiquoter in patterns"
quoteTmplExp s = return tmpl
  where
    vars = case parseSTMPNames s of
             Right xs -> xs
             Left  err -> error "err"
    base  = TH.AppE (TH.VarE (TH.mkName "newSTMP")) (TH.LitE (TH.StringL $ s))
    tmpl  = foldr addAttrib base vars
    addAttrib var x = TH.AppE
        (TH.AppE (TH.AppE (TH.VarE (TH.mkName "setAttribute"))
                          (TH.LitE (TH.StringL ('`' : var ++ "`"))))
                 (TH.VarE (TH.mkName  var)))
        x