module Api.Database.Handlers (
    markovNamesHandler
) where

import Servant

import Api.Database.ApiType
import Api.Database.Models
import Api.Helpers
import MarkovDatabase

markovNamesHandler :: MarkovDatabase String -> Handler MarkovNames
markovNamesHandler markov =
    let keysFetching = fmap MarkovNames markovNames
    in toHandlerWithDatabase keysFetching markov