{-# LANGUAGE
    OverloadedStrings
#-}

module Api.Helpers (
    toHandlerWithDatabase
) where

import Control.Monad.Trans (MonadIO(..))
import Data.ByteString.Lazy.Char8 (pack)
import Servant

import MarkovDatabase

toHandlerWithDatabase :: MarkovDatabaseMonad a b -> MarkovDatabase a -> Handler b
toHandlerWithDatabase action markov = do
    result <- liftIO $ runMarkovDatabaseMonad action markov
    case result of
        Left (MarkovNotFound entity) -> throwError $ err404 {errBody = "markov map '" <> pack entity <> "' does not exist"}
        Right result' -> pure result'