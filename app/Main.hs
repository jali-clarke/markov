module Main where

import Control.Concurrent (MVar, newMVar)
import Network.Wai.Handler.Warp
import Servant
import System.Environment (getArgs)
import Text.Read (readMaybe)

import Api
import Handlers
import Markov

server :: MVar (Markov String) -> Server Api
server markov = generateMessageHandler markov :<|> trainHandler markov :<|> calibrateHandler markov

main :: IO ()
main = do
    args <- getArgs
    case args of
        [portString] ->
            case readMaybe portString of
                Nothing -> putStrLn "invalid port"
                Just port -> do
                    markov <- newMVar emptyMarkov
                    let application = serve api (server markov)
                    run port application
        _ -> putStrLn "usage: markov <port>"