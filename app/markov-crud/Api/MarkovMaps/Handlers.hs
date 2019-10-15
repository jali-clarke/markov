module Api.MarkovMaps.Handlers (
    markovMapsHandler
) where

import qualified Control.Monad.Except as MTL
import Data.Text (Text)
import Servant

import Api.ModelHelpers
import Api.MarkovMaps.Types
import MarkovDatabase

markovNamesHandler :: MarkovDatabaseBackend m => ServerT MarkovMapGetAll (MarkovDatabaseMonad a m)
markovNamesHandler = fmap (ListOf . fmap markovMapWithHref) markovNames

markovCreateHandler :: MarkovDatabaseBackend m => ServerT MarkovMapCreate (MarkovDatabaseMonad a m)
markovCreateHandler (MarkovMap markovName _) = NoContent <$ createMarkov markovName

markovMapsManyHandler :: MarkovDatabaseBackend m => ServerT MarkovMapsManyApi (MarkovDatabaseMonad a m)
markovMapsManyHandler = markovNamesHandler :<|> markovCreateHandler

markovNameHandler :: MarkovDatabaseBackend m => Text -> ServerT MarkovMapGetOne (MarkovDatabaseMonad a m)
markovNameHandler markovName = do
    exists <- markovExists markovName
    if exists
        then pure $ markovMapWithHref markovName
        else MTL.throwError $ MarkovNotFound markovName

processTrainingMessages :: MarkovDatabaseBackend m => Text -> TrainingMessages -> MarkovDatabaseMonad String m ()
processTrainingMessages markovName (TrainingMessages trainingMessages) =
    processIntoMarkov markovName (fmap words trainingMessages)

trainHandler :: MarkovDatabaseBackend m => Text -> ServerT MarkovMapTrain (MarkovDatabaseMonad String m)
trainHandler markovName trainingMessages = NoContent <$ processTrainingMessages markovName trainingMessages

deletionHandler :: MarkovDatabaseBackend m => Text -> ServerT MarkovMapDelete (MarkovDatabaseMonad a m)
deletionHandler markovName = NoContent <$ deleteMarkov markovName

markovMapsOneHandler :: MarkovDatabaseBackend m => Text -> ServerT MarkovMapsOneApi (MarkovDatabaseMonad String m)
markovMapsOneHandler markovName =
    markovNameHandler markovName :<|> trainHandler markovName :<|> deletionHandler markovName

markovMapsHandler :: MarkovDatabaseBackend m => ServerT MarkovMapsApi (MarkovDatabaseMonad String m)
markovMapsHandler = markovMapsManyHandler :<|> markovMapsOneHandler