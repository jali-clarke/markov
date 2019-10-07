module Api.Server.Database.Handlers (
    markovNamesHandler
) where

import Api.Database
import MarkovDatabase

markovNamesHandler :: MarkovDatabaseBackend m => MarkovDatabaseMonad a m MarkovNames
markovNamesHandler = fmap MarkovNames markovNames