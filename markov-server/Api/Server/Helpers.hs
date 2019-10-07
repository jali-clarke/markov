{-# LANGUAGE
    OverloadedStrings
#-}

module Api.Server.Helpers (
    toHandlerWithDatabase
) where

import Control.Monad.Trans (MonadIO(..))
import Data.ByteString.Lazy.Char8 (pack)
import Servant

import InMemoryBackend
import MarkovDatabase

toHandlerWithDatabase :: MarkovDatabaseMonad a InMemoryBackend b -> InMemoryDB -> Handler b
toHandlerWithDatabase action markov = do
    result <- liftIO $ hoistBackendAndRun (runWithInMemoryDB markov) action
    case result of
        Left err ->
            case err of
                MarkovNotFound entity -> throwError $ err404 {errBody = "markov map '" <> pack entity <> "' does not exist"}
                CorruptedData rawData -> do
                    liftIO . putStrLn $ "corrupted data: " ++ show rawData
                    throwError $ err503 {errBody = "corrupted data"}
        Right result' -> pure result'