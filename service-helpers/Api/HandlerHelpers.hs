{-# LANGUAGE
    OverloadedStrings,
    RankNTypes
#-}

module Api.HandlerHelpers (
    hoistToHandler
) where

import Control.Monad.Trans (MonadIO(..))
import qualified Data.ByteString.Builder as B
import qualified Data.Text.Encoding as Text
import Servant

import MarkovDatabase
import MarkovDatabaseBackend

handleError :: DatabaseError -> Handler a
handleError err =
    case err of
        MarkovNotFound entity ->
            let message = B.lazyByteString "markov map '" <> Text.encodeUtf8Builder entity <> B.lazyByteString "' does not exist"
            in throwError $ err404 {errBody = B.toLazyByteString message}
        CorruptedData rawData -> do
            liftIO . putStrLn $ "corrupted data: " ++ show rawData
            throwError $ err500 {errBody = "corrupted data"}
        BackendFailure message -> do
            liftIO . putStrLn $ "backend failure: " ++ message
            throwError $ err500 {errBody = "backend failure"}

hoistToHandler :: MarkovDatabaseBackend m => (forall x. m x -> IO (Either BackendError x)) -> MarkovDatabaseMonad a m b -> Handler b
hoistToHandler interpreter action = do
    result <- liftIO $ hoistBackendAndRun interpreter action
    case result of
        Left err -> handleError err
        Right result' -> pure result'
