module Api.Handlers (
    apiHandler
) where

import Servant

import Api.MarkovMaps.Handlers
import Api.Types
import MarkovDatabase

apiHandler :: MarkovDatabaseBackend m => ServerT Api (MarkovDatabaseMonad a m)
apiHandler = markovMapsHandler