{-# LANGUAGE
    OverloadedStrings
#-}

module Handlers (
    generateMessageHandler,
    trainHandler,
    calibrateHandler,
    deletionHandler
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

generateMessageHandler :: MarkovDatabase String -> String -> Handler GeneratedMessage
generateMessageHandler markov markovName =
    let sentenceGenerator = fmap (GeneratedMessage . unwords) (generateSentence markovName)
    in toHandlerWithDatabase sentenceGenerator markov

insertTrainingMessages :: String -> TrainingMessages -> MarkovDatabaseMonad String ()
insertTrainingMessages markovName (TrainingMessages messages) = insertIntoMarkov markovName (fmap words messages)

trainHandler :: MarkovDatabase String -> String -> TrainingMessages -> Handler NoContent
trainHandler markov markovName messages =
    let initAndInsertion = makeNewMarkov markovName *> insertTrainingMessages markovName messages
    in toHandlerWithDatabase (NoContent <$ initAndInsertion) markov

calibrateHandler :: MarkovDatabase String -> String -> TrainingMessages -> Handler NoContent
calibrateHandler markov markovName messages =
    let insertion = insertTrainingMessages markovName messages
    in toHandlerWithDatabase (NoContent <$ insertion) markov

deletionHandler :: MarkovDatabase String -> String -> Handler NoContent
deletionHandler markov markovName =
    let deletion = deleteMarkov markovName
    in toHandlerWithDatabase (NoContent <$ deletion) markov