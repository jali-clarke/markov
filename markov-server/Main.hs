module Main where

import Network.Wai.Handler.Warp
import Servant
import System.Environment (getArgs)
import Text.Read (readMaybe)

import Api
import Api.Server
import Api.Server.Helpers
import InMemoryBackend

main :: IO ()
main = do
    args <- getArgs
    case args of
        [portString] ->
            case readMaybe portString of
                Nothing -> putStrLn "invalid port"
                Just port -> do
                    markov <- emptyInMemoryDB
                    let application = serve api $ hoistServer api (toHandlerWithDatabase markov) apiServer
                    run port application
        _ -> putStrLn "usage: markov <port>"