module Handlers (
    generateMessageHandler,
    trainHandler,
    calibrateHandler
) where

import Control.Concurrent.MVar (MVar, readMVar, putMVar, swapMVar, takeMVar)
import Control.Monad.Trans (MonadIO(..))
import Servant

import Api
import Markov

generateMessageHandler :: MVar (Markov String) -> Handler GeneratedMessage
generateMessageHandler markov = liftIO $ readMVar markov >>= fmap (GeneratedMessage . unwords) . generateSentence

trainHandler :: MVar (Markov String) -> TrainingMessages -> Handler NoContent
trainHandler = modifyingHandler (const emptyMarkov)

calibrateHandler :: MVar (Markov String) -> TrainingMessages -> Handler NoContent
calibrateHandler = modifyingHandler id

modifyingHandler :: (Markov String -> Markov String) -> MVar (Markov String) -> TrainingMessages -> Handler NoContent
modifyingHandler modifier markov (TrainingMessages messagesToTrain) = liftIO . (NoContent <$) $ do
    oldMarkov <- takeMVar markov
    putMVar markov $ trainMarkovOnSentences (fmap words messagesToTrain) (modifier oldMarkov)