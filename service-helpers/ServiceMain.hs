{-# LANGUAGE
    DataKinds,
    FlexibleContexts
#-}

module ServiceMain (
    serviceMain
) where

import Data.Proxy
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger
import Servant
import System.Environment (getArgs, lookupEnv)
import Text.Read (readMaybe)

import Api.HandlerHelpers
import CassandraBackend
import MarkovDatabase

cassandraHost :: IO String
cassandraHost = do
    maybeHost <- lookupEnv "CASSANDRA_HOST"
    pure $ case maybeHost of
        Nothing -> "localhost"
        Just "" -> "localhost"
        Just host -> host

startServer :: HasServer a '[] => Proxy a -> ServerT a (MarkovDatabaseMonad b CassandraBackend) -> Int -> IO ()
startServer api server port = do
    dbHost <- cassandraHost
    clientState <- clientInitState dbHost
    let interpreter = hoistToHandler (runCassandraBackend clientState)
        application = logStdout . serve api $ hoistServer api interpreter server
    run port application

serviceMain :: HasServer a '[] => String -> Proxy a -> ServerT a (MarkovDatabaseMonad b CassandraBackend) -> IO ()
serviceMain name api server = do
    args <- getArgs
    case args of
        [portString] -> maybe (putStrLn "invalid port") (startServer api server) (readMaybe portString)
        _ -> putStrLn $ "usage: " ++ name ++ " <port>"