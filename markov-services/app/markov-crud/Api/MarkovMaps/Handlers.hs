{-# LANGUAGE
    OverloadedStrings
#-}

module Api.MarkovMaps.Handlers (
    markovMapsHandler
) where

import Control.Monad (when)
import qualified Control.Monad.Except as MTL
import qualified Data.Set as Set
import qualified Data.Text as Text
import Servant

import Api.ModelHelpers
import Api.MarkovMaps.Types
import MarkovDatabase

markovNamesHandler :: MarkovDatabaseBackend m => ServerT MarkovMapGetAll (MarkovDatabaseMonad a m)
markovNamesHandler = fmap (ListOf . fmap markovMapWithHref) markovNames

allowedCharacters :: Set.Set Char
allowedCharacters = Set.fromList $ ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "-_.~"

nameIsInvalid :: Text.Text -> Bool
nameIsInvalid = Text.any (flip Set.notMember allowedCharacters)

markovCreateHandler :: MarkovDatabaseBackend m => ServerT MarkovMapCreate (MarkovDatabaseMonad a m)
markovCreateHandler (MarkovMap markovName _) = NoContent <$ do
    when (nameIsInvalid markovName) $ MTL.throwError (BadRequest $ "markov name '" <> markovName <> "' must be url-encoded")
    createMarkov markovName

markovMapsManyHandler :: MarkovDatabaseBackend m => ServerT MarkovMapsManyApi (MarkovDatabaseMonad a m)
markovMapsManyHandler = markovNamesHandler :<|> markovCreateHandler

markovNameHandler :: MarkovDatabaseBackend m => Text.Text -> ServerT MarkovMapGetOne (MarkovDatabaseMonad a m)
markovNameHandler markovName = do
    exists <- markovExists markovName
    if exists
        then pure $ markovMapWithHref markovName
        else MTL.throwError $ MarkovNotFound markovName

processTrainingMessages :: MarkovDatabaseBackend m => Text.Text -> TrainingMessages -> MarkovDatabaseMonad Text.Text m ()
processTrainingMessages markovName (TrainingMessages trainingMessages) =
    processIntoMarkov markovName (fmap Text.words trainingMessages)

trainHandler :: MarkovDatabaseBackend m => Text.Text -> ServerT MarkovMapTrain (MarkovDatabaseMonad Text.Text m)
trainHandler markovName trainingMessages = NoContent <$ processTrainingMessages markovName trainingMessages

deletionHandler :: MarkovDatabaseBackend m => Text.Text -> ServerT MarkovMapDelete (MarkovDatabaseMonad a m)
deletionHandler markovName = NoContent <$ deleteMarkov markovName

markovMapsOneHandler :: MarkovDatabaseBackend m => Text.Text -> ServerT MarkovMapsOneApi (MarkovDatabaseMonad Text.Text m)
markovMapsOneHandler markovName =
    markovNameHandler markovName :<|> trainHandler markovName :<|> deletionHandler markovName

markovMapsHandler :: MarkovDatabaseBackend m => ServerT MarkovMapsApi (MarkovDatabaseMonad Text.Text m)
markovMapsHandler = markovMapsManyHandler :<|> markovMapsOneHandler