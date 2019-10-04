module Api.Server.Database (
    databaseServer
) where

import Servant

import Api.Database
import Api.Server.Database.Handlers
import MarkovDatabase

databaseServer :: MarkovDatabase String -> Server DatabaseApi
databaseServer markov = markovNamesHandler markov