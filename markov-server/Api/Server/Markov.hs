module Api.Server.Markov (
    markovServer
) where

import Servant

import Api.Markov
import Api.Server.Markov.Handlers
import MarkovDatabase

markovServer :: MarkovDatabaseBackend m => String -> ServerT MarkovApi (MarkovDatabaseMonad a m)
markovServer markovName =
    let trainAndRecalibrateHandler markovName' trainingMessages =
            trainHandler markovName' trainingMessages
            :<|> calibrateHandler markovName' trainingMessages
    in generateMessageHandler markovName
        :<|> trainAndRecalibrateHandler markovName
        :<|> deletionHandler markovName