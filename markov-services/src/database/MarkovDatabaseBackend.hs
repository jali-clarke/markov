{-# LANGUAGE
    FlexibleContexts
#-}

module MarkovDatabaseBackend (
    BackendError(..),
    MarkovDatabaseBackend(..)
) where

import qualified Control.Monad.Except as MTL
import qualified Data.ByteString.Lazy as B
import Data.Text (Text)

data BackendError = MarkovNotFoundBackend Text | OtherError String

class (MTL.MonadError BackendError m, MTL.MonadIO m) => MarkovDatabaseBackend m where
    backendCreateMarkov :: Text -> m ()
    backendDeleteMarkov :: Text -> m ()
    backendMarkovExists :: Text -> m Bool
    backendMarkovNames :: m [Text]

    backendGetMarkovCounts :: Text -> m [(B.ByteString, B.ByteString, Int)]
    backendIncrementMarkovCounts :: Text -> [(B.ByteString, B.ByteString)] -> m ()