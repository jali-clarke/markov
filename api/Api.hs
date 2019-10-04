{-# LANGUAGE
    DataKinds,
    TypeOperators
#-}

module Api (
    Api,
    api
) where

import Data.Proxy
import Servant.API

import Api.Database
import Api.Markov

type Api = DatabaseApi :<|> (Capture "markovName" String :> MarkovApi)

api :: Proxy Api
api = Proxy