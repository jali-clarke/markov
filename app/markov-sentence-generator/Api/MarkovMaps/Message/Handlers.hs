module Api.MarkovMaps.Message.Handlers (
    generateMessageHandler
) where

import Control.Monad.Trans (MonadIO(..))
import qualified Data.Text as Text
import Servant

import Api.MarkovMaps.Message.Types
import MarkovDatabase
import SentenceGeneration

generateMessageHandler :: MarkovDatabaseBackend m => ServerT MessageApi (MarkovDatabaseMonad Text.Text m)
generateMessageHandler markovName = do
    corpus <- getCorpus markovName
    liftIO $ fmap (GeneratedMessage . Text.unwords) (generateSentence corpus)
