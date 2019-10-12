module Main where

import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger
import Servant
import System.Environment (getArgs, lookupEnv)
import Text.Read (readMaybe)

import Api.HandlerHelpers
import Api.MarkovMaps.Message.Handlers
import Api.MarkovMaps.Message.Types
import CassandraBackend

cassandraHost :: IO String
cassandraHost = do
    maybeHost <- lookupEnv "CASSANDRA_HOST"
    pure $ case maybeHost of
        Nothing -> "localhost"
        Just "" -> "localhost"
        Just host -> host

startServer :: Int -> IO ()
startServer port = do
    dbHost <- cassandraHost
    clientState <- clientInitState dbHost
    let interpreter = hoistToHandler (runCassandraBackend clientState)
        application = logStdout . serve messageApi $ hoistServer messageApi interpreter generateMessageHandler
    run port application

main :: IO ()
main = do
    args <- getArgs
    case args of
        [portString] -> maybe (putStrLn "invalid port") startServer (readMaybe portString)
        _ -> putStrLn "usage: markov-sentence-generator <port>"