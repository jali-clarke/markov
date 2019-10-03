{-# LANGUAGE
    DataKinds
#-}

module Api.Database.ApiType (
    DatabaseApi
) where

import Data.Proxy
import Servant.API

import Api.Database.Models

type DatabaseApi = Get '[JSON] MarkovNames