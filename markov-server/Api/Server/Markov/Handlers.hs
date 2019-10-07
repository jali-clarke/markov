module Api.Server.Markov.Handlers (
    generateMessageHandler,
    trainHandler,
    calibrateHandler,
    deletionHandler
) where

import Control.Monad.Trans (MonadIO(..))
import Servant

import Api.Markov
import MarkovDatabase
import SentenceGeneration

generateMessageHandler :: MarkovDatabaseBackend m => String -> MarkovDatabaseMonad a m GeneratedMessage
generateMessageHandler markovName = do
    corpus <- getCorpus markovName
    liftIO $ fmap (GeneratedMessage . unwords) (generateSentence corpus)

processTrainingMessages :: MarkovDatabaseBackend m => String -> TrainingMessages -> MarkovDatabaseMonad a m ()
processTrainingMessages markovName (TrainingMessages trainingMessages) =
    processIntoMarkov markovName (fmap words trainingMessages)

trainHandler :: MarkovDatabaseBackend m => String -> TrainingMessages -> MarkovDatabaseMonad a m NoContent
trainHandler markovName trainingMessages = (NoContent <$) $
    deleteMarkov markovName
    *> createMarkov markovName
    *> processTrainingMessages markovName trainingMessages

calibrateHandler :: MarkovDatabaseBackend m => String -> TrainingMessages -> MarkovDatabaseMonad a m NoContent
calibrateHandler markovName trainingMessages = NoContent <$ processTrainingMessages markovName trainingMessages

deletionHandler :: MarkovDatabaseBackend m => String -> MarkovDatabaseMonad a m NoContent
deletionHandler markovName = NoContent <$ deleteMarkov markovName
