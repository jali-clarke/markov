module Api.Server.Database.Handlers (
    markovNamesHandler
) where

import Servant

import Api.Database
import Api.Server.Helpers
import MarkovDatabase

markovNamesHandler :: MarkovDatabase String -> Handler MarkovNames
markovNamesHandler markov =
    let keysFetching = fmap MarkovNames markovNames
    in toHandlerWithDatabase keysFetching markov