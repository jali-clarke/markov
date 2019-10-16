{-# LANGUAGE
    DeriveGeneric
#-}

module Api.ModelHelpers where

import Data.Aeson
import GHC.Generics

data ListOf a = ListOf {items :: [a]} deriving Generic
instance ToJSON a => ToJSON (ListOf a)