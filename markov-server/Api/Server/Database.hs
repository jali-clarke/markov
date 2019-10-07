module Api.Server.Database (
    databaseServer
) where

import Servant

import Api.Database
import Api.Server.Database.Handlers
import MarkovDatabase

databaseServer :: MarkovDatabaseBackend m => ServerT DatabaseApi (MarkovDatabaseMonad a m)
databaseServer = markovNamesHandler