module Api.Server.Markov.Handlers (
    generateMessageHandler,
    trainHandler,
    calibrateHandler,
    deletionHandler
) where

import Control.Monad.Trans (MonadIO(..))
import Servant

import Api.Markov
import Api.Server.Helpers
import MarkovDatabase
import SentenceGeneration

generateMessageHandler :: MarkovDatabase String -> String -> Handler GeneratedMessage
generateMessageHandler markov markovName = do
    corpus <- toHandlerWithDatabase (getCorpus markovName) markov
    let sentenceGenerator = fmap (GeneratedMessage . unwords) . generateSentence $ corpus
    liftIO sentenceGenerator

insertTrainingMessages :: String -> TrainingMessages -> MarkovDatabaseMonad String ()
insertTrainingMessages markovName (TrainingMessages trainingMessages) =
    insertIntoMarkov markovName (fmap words trainingMessages)

trainHandler :: MarkovDatabase String -> String -> TrainingMessages -> Handler NoContent
trainHandler markov markovName trainingMessages =
    let initAndInsertion = makeNewMarkov markovName *> insertTrainingMessages markovName trainingMessages
    in toHandlerWithDatabase (NoContent <$ initAndInsertion) markov

calibrateHandler :: MarkovDatabase String -> String -> TrainingMessages -> Handler NoContent
calibrateHandler markov markovName trainingMessages =
    let insertion = insertTrainingMessages markovName trainingMessages
    in toHandlerWithDatabase (NoContent <$ insertion) markov

deletionHandler :: MarkovDatabase String -> String -> Handler NoContent
deletionHandler markov markovName =
    let deletion = deleteMarkov markovName
    in toHandlerWithDatabase (NoContent <$ deletion) markov
