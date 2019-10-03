{-# LANGUAGE
    DataKinds,
    DeriveGeneric,
    TypeOperators
#-}

module Api (
    Api,
    apiServer,
    api
) where

import Servant

import Api.Database
import Api.Markov
import MarkovDatabase

type Api = DatabaseApi :<|> (Capture "markovName" String :> MarkovApi)

apiServer :: MarkovDatabase String -> Server Api
apiServer markov = databaseServer markov :<|> markovServer markov

api :: Proxy Api
api = Proxy