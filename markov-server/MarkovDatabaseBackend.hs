{-# LANGUAGE
    FlexibleContexts
#-}

module MarkovDatabaseBackend (
    BackendError(..),
    MarkovDatabaseBackend(..)
) where

import qualified Control.Monad.Except as MTL
import qualified Data.ByteString.Lazy as B

data BackendError = MarkovNotFoundBackend String

class (MTL.MonadError BackendError m, MTL.MonadIO m) => MarkovDatabaseBackend m where
    backendCreateMarkov :: String -> m ()
    backendDeleteMarkov :: String -> m ()
    backendExistsMarkov :: String -> m Bool
    backendMarkovNames :: m [String]

    backendGetMarkovCounts :: String -> m [(B.ByteString, B.ByteString, Int)]
    backendIncrementMarkovCounts :: String -> [(B.ByteString, B.ByteString)] -> m ()