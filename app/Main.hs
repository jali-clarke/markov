module Main where

import Data.Functor (void)
import Network.Wai.Handler.Warp
import Servant
import System.Environment (getArgs)
import Text.Read (readMaybe)

import Api
import Handlers
import MarkovDatabase

server :: MarkovDatabase String -> Server Api
server markov =
    let trainingAndCalibrationServer markov' trainingMessages =
            trainHandler markov' trainingMessages :<|> calibrateHandler markov' trainingMessages
    in generateMessageHandler markov :<|> trainingAndCalibrationServer markov

main :: IO ()
main = do
    args <- getArgs
    case args of
        [portString] ->
            case readMaybe portString of
                Nothing -> putStrLn "invalid port"
                Just port -> do
                    markov <- emptyDatabase
                    void $ runMarkovDatabaseMonad (makeNewMarkov "cool_database") markov
                    let application = serve api (server markov)
                    run port application
        _ -> putStrLn "usage: markov <port>"