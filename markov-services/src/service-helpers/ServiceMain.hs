{-# LANGUAGE
    DataKinds,
    FlexibleContexts
#-}

module ServiceMain (
    serviceMain
) where

import Data.Proxy
import Data.Text (Text)
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

startServer :: (String -> IO Application) -> Int -> IO ()
startServer app port = do
    dbHost <- cassandraHost
    application <- fmap logStdout (app dbHost)
    run port application

appFromApi :: HasServer a '[] => Proxy a -> ServerT a (MarkovDatabaseMonad Text CassandraBackend) -> String -> IO Application
appFromApi api server = \dbHost -> do
    clientState <- clientInitState dbHost
    let interpreter = hoistToHandler (runCassandraBackend clientState)
    pure $ serve api $ hoistServer api interpreter server

serviceMain :: HasServer a '[] => String -> Proxy a -> ServerT a (MarkovDatabaseMonad Text CassandraBackend) -> IO ()
serviceMain name api server = do
    args <- getArgs
    case args of
        [portString] ->
            let app = appFromApi api server
            in maybe (putStrLn "invalid port") (startServer app) (readMaybe portString)
        _ -> putStrLn $ "usage: " ++ name ++ " <port>"