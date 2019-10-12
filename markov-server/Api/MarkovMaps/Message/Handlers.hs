module Api.MarkovMaps.Message.Handlers (
    generateMessageHandler
) where

import Control.Monad.Trans (MonadIO(..))
import Data.Text (Text)
import Servant

import Api.MarkovMaps.Message.Types
import MarkovDatabase
import SentenceGeneration

generateMessageHandler :: MarkovDatabaseBackend m => Text -> ServerT MessageApi (MarkovDatabaseMonad a m)
generateMessageHandler markovName = do
    corpus <- getCorpus markovName
    liftIO $ fmap (GeneratedMessage . unwords) (generateSentence corpus)
