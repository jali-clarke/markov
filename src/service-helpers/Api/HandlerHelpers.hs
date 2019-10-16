{-# LANGUAGE
    OverloadedStrings,
    RankNTypes
#-}

module Api.HandlerHelpers (
    hoistToHandler
) where

import Control.Monad.Trans (MonadIO(..))
import qualified Data.ByteString.Builder as B
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Servant

import MarkovDatabase
import MarkovDatabaseBackend

handleError :: DatabaseError -> Handler a
handleError err =
    case err of
        BackendFailure message -> do
            liftIO . putStrLn $ "backend failure: " ++ message
            throwError $ err500 {errBody = "backend failure"}
        BadRequest message ->
            let encodedMessage = Text.encodeUtf8Builder message
            in throwError $ err400 {errBody = "bad request: " <> B.toLazyByteString encodedMessage}
        CorruptedData rawData -> do
            liftIO . putStrLn $ "corrupted data: " ++ show rawData
            throwError $ err500 {errBody = "corrupted data"}
        MarkovNotFound markovName ->
            let message = B.lazyByteString "markov map '" <> Text.encodeUtf8Builder markovName <> B.lazyByteString "' does not exist"
            in throwError $ err404 {errBody = B.toLazyByteString message}

hoistToHandler :: MarkovDatabaseBackend m => (forall x. m x -> IO (Either BackendError x)) -> MarkovDatabaseMonad Text.Text m b -> Handler b
hoistToHandler interpreter action = do
    result <- liftIO $ hoistBackendAndRun interpreter action
    case result of
        Left err -> handleError err
        Right result' -> pure result'
