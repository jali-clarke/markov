module Api.MarkovMaps.Message.Handlers (
    generateMessageHandler
) where

import Control.Monad.Trans (MonadIO(..))
import Servant

import Api.MarkovMaps.Message.Types
import MarkovDatabase
import SentenceGeneration

generateMessageHandler :: MarkovDatabaseBackend m => ServerT MessageApi (MarkovDatabaseMonad String m)
generateMessageHandler markovName = do
    corpus <- getCorpus markovName
    liftIO $ fmap (GeneratedMessage . unwords) (generateSentence corpus)
