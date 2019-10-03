{-# LANGUAGE
    DeriveGeneric
#-}

module Api.Database.Models (
    MarkovNames(..)
) where

import Data.Aeson
import GHC.Generics

data MarkovNames = MarkovNames {names :: [String]} deriving Generic
instance ToJSON MarkovNames
    