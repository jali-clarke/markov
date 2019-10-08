{-# LANGUAGE
    OverloadedStrings,
    RankNTypes
#-}

module Api.Server.Helpers (
    hoistToHandler
) where

import Control.Monad.Trans (MonadIO(..))
import Data.ByteString.Lazy.Char8 (pack)
import Servant

import MarkovDatabase
import MarkovDatabaseBackend

hoistToHandler :: MarkovDatabaseBackend m => (forall x. m x -> IO (Either BackendError x)) -> MarkovDatabaseMonad a m b -> Handler b
hoistToHandler interpreter action = do
    result <- liftIO $ hoistBackendAndRun interpreter action
    case result of
        Left err ->
            case err of
                MarkovNotFound entity -> throwError $ err404 {errBody = "markov map '" <> pack entity <> "' does not exist"}
                CorruptedData rawData -> do
                    liftIO . putStrLn $ "corrupted data: " ++ show rawData
                    throwError $ err503 {errBody = "corrupted data"}
        Right result' -> pure result'
