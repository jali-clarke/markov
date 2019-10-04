module Api.Server (
    apiServer
) where

import Servant

import Api
import Api.Server.Database
import Api.Server.Markov
import MarkovDatabase

apiServer :: MarkovDatabase String -> Server Api
apiServer markov = databaseServer markov :<|> markovServer markov
