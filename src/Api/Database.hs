module Api.Database (
    DatabaseApi,
    databaseServer,
    databaseApi
) where

import Data.Proxy
import Servant

import Api.Database.ApiType
import Api.Database.Handlers
import MarkovDatabase

databaseServer :: MarkovDatabase String -> Server DatabaseApi
databaseServer markov = markovNamesHandler markov

databaseApi :: Proxy DatabaseApi
databaseApi = Proxy