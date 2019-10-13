{-# LANGUAGE
    DataKinds,
    FlexibleContexts
#-}

module ServiceMain (
    serviceMain
) where

import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger
import Servant
import System.Environment (getArgs, lookupEnv)
import Text.Read (readMaybe)

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

serviceMain :: String -> (String -> IO Application) -> IO ()
serviceMain name app = do
    args <- getArgs
    case args of
        [portString] -> maybe (putStrLn "invalid port") (startServer app) (readMaybe portString)
        _ -> putStrLn $ "usage: " ++ name ++ " <port>"