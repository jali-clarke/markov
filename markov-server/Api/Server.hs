module Api.Server (
    apiServer
) where

import Servant

import Api
import Api.Server.Database
import Api.Server.Markov
import MarkovDatabase

apiServer :: MarkovDatabaseBackend m => ServerT Api (MarkovDatabaseMonad a m)
apiServer = databaseServer :<|> markovServer
