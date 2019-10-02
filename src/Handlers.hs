{-# LANGUAGE
    OverloadedStrings
#-}

module Handlers (
    generateMessageHandler,
    trainHandler,
    calibrateHandler
) where

import Control.Monad.Trans (MonadIO(..))
import Data.ByteString.Lazy.Char8 (pack)
import Servant

import Api
import MarkovDatabase

toHandlerWithDatabase :: MarkovDatabaseMonad a b -> MarkovDatabase a -> Handler b
toHandlerWithDatabase action markov = do
    result <- liftIO $ runMarkovDatabaseMonad action markov
    case result of
        Left (MarkovNotFound entity) -> throwError $ err404 {errBody = "markov map '" <> pack entity <> "' does not exist"}
        Right result' -> pure result'

generateMessageHandler :: MarkovDatabase String -> Handler GeneratedMessage
generateMessageHandler markov =
    let sentenceGenerator = fmap (GeneratedMessage . unwords) (generateSentence "cool_database")
    in toHandlerWithDatabase sentenceGenerator markov

insertTrainingMessages :: String -> TrainingMessages -> MarkovDatabaseMonad String ()
insertTrainingMessages markovName (TrainingMessages messages) = insertIntoMarkov markovName (fmap words messages)

trainHandler :: MarkovDatabase String -> TrainingMessages -> Handler NoContent
trainHandler markov messages =
    let initAndInsertion = makeNewMarkov "cool_database" *> insertTrainingMessages "cool_database" messages
    in toHandlerWithDatabase (NoContent <$ initAndInsertion) markov

calibrateHandler :: MarkovDatabase String -> TrainingMessages -> Handler NoContent
calibrateHandler markov messages =
    let insertion = insertTrainingMessages "cool_database" messages
    in toHandlerWithDatabase (NoContent <$ insertion) markov