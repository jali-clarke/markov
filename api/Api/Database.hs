{-# LANGUAGE
    DataKinds,
    DeriveGeneric
#-}

module Api.Database where

import Data.Aeson
import Data.Proxy
import GHC.Generics
import Servant.API

data MarkovNames = MarkovNames {names :: [String]} deriving Generic
instance ToJSON MarkovNames
instance FromJSON MarkovNames

type DatabaseApi = Get '[JSON] MarkovNames

databaseApi :: Proxy DatabaseApi
databaseApi = Proxy