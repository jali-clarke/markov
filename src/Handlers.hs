module Handlers (
    generateMessageHandler,
    trainHandler,
    calibrateHandler,
    deletionHandler,
    databaseServer
) where

import Control.Monad.Trans (MonadIO(..))
import Servant

import Api
import Api.Database
import Api.Helpers
import MarkovDatabase
import SentenceGeneration

generateMessageHandler :: MarkovDatabase String -> String -> Handler GeneratedMessage
generateMessageHandler markov markovName = do
    corpus <- toHandlerWithDatabase (getCorpus markovName) markov
    let sentenceGenerator = fmap (GeneratedMessage . unwords) . generateSentence $ corpus
    liftIO sentenceGenerator

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
