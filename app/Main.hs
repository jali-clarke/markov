module Main where

import Network.Wai.Handler.Warp
import Servant
import System.Environment (getArgs)
import Text.Read (readMaybe)

import Api
import Handlers
import MarkovDatabase

server :: MarkovDatabase String -> Server Api
server markov =
    let trainAndCalibrateHandler markovName trainMessages =
            trainHandler markov markovName trainMessages
            :<|> calibrateHandler markov markovName trainMessages
        
        markovServer markovName =
            generateMessageHandler markov markovName
            :<|> trainAndCalibrateHandler markovName
            :<|> deletionHandler markov markovName
    in markovServer

main :: IO ()
main = do
    args <- getArgs
    case args of
        [portString] ->
            case readMaybe portString of
                Nothing -> putStrLn "invalid port"
                Just port -> do
                    markov <- emptyDatabase
                    let application = serve api (server markov)
                    run port application
        _ -> putStrLn "usage: markov <port>"