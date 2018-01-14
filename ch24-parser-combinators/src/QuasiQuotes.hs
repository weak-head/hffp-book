{-# LANGUAGE QuasiQuotes #-}
-- https://wiki.haskell.org/Quasiquotation


module QuasiQuotes where

import Control.Applicative
import Text.RawString.QQ
import Text.Trifecta

type NumberOrString =
  Either Integer String

eitherOr :: String
eitherOr = [r|
             123
             abc
             456
             def
             |]

{-

>> stack ghci --with-ghc ghci parser-combinators

> :set -ddump-splices
> :l src/QuasiQuotes.hs

-}
