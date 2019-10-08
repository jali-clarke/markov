module Main where

import Network.Wai.Handler.Warp
import Servant
import System.Environment (getArgs)
import Text.Read (readMaybe)

import Api
import Api.Server
import Api.Server.Helpers
import InMemoryBackend

startServer :: Int -> IO ()
startServer port = do
    markov <- emptyInMemoryDB
    let interpreter = hoistToHandler (runWithInMemoryDB markov)
        application = serve api $ hoistServer api interpreter apiServer
    run port application

main :: IO ()
main = do
    args <- getArgs
    case args of
        [portString] -> maybe (putStrLn "invalid port") startServer (readMaybe portString)
        _ -> putStrLn "usage: markov <port>"