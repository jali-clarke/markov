module Api.Server.Markov (
    markovServer
) where

import Servant

import Api.Markov
import Api.Server.Markov.Handlers
import MarkovDatabase

markovServer :: MarkovDatabase String -> String -> Server MarkovApi
markovServer markov markovName =
    let trainAndRecalibrateHandler markov' markovName' trainingMessages =
            trainHandler markov' markovName' trainingMessages
            :<|> calibrateHandler markov' markovName' trainingMessages
    in generateMessageHandler markov markovName
        :<|> trainAndRecalibrateHandler markov markovName
        :<|> deletionHandler markov markovName