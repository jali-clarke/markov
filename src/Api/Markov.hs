module Api.Markov (
    MarkovApi,
    markovServer,
    markovApi
) where

import Data.Proxy
import Servant

import Api.Markov.ApiType
import Api.Markov.Handlers
import MarkovDatabase

markovServer :: MarkovDatabase String -> String -> Server MarkovApi
markovServer markov markovName =
    let trainAndRecalibrateHandler markov' markovName' trainingMessages =
            trainHandler markov' markovName' trainingMessages
            :<|> calibrateHandler markov' markovName' trainingMessages
    in generateMessageHandler markov markovName
        :<|> trainAndRecalibrateHandler markov markovName
        :<|> deletionHandler markov markovName

markovApi :: Proxy MarkovApi
markovApi = Proxy