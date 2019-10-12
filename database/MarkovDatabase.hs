{-# LANGUAGE
    RankNTypes,
    TupleSections
#-}

module MarkovDatabase (
    MarkovDatabaseBackend(..),
    DatabaseError(..),
    MarkovDatabaseMonad,
    hoistBackendAndRun,

    createMarkov,
    deleteMarkov,
    markovExists,
    markovNames,

    processIntoMarkov,
    getCorpus
) where

import qualified Control.Monad.Except as MTL
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as M
import Data.Text (Text)

import Corpus
import MarkovToken
import MarkovDatabaseBackend
import Serializable

data DatabaseError = MarkovNotFound Text | CorruptedData B.ByteString | BackendFailure String
type MarkovDatabaseMonad a m = MTL.ExceptT DatabaseError m

translateBackendError :: BackendError -> DatabaseError
translateBackendError err =
    case err of
        MarkovNotFoundBackend markovName -> MarkovNotFound markovName
        OtherError message -> BackendFailure message

hoistBackendAndRun :: (MarkovDatabaseBackend m, Monad n) => (forall x. m x -> n (Either BackendError x)) -> MarkovDatabaseMonad a m b -> n (Either DatabaseError b)
hoistBackendAndRun interpreter action = do
    potentialBackendError <- interpreter (MTL.runExceptT action)
    pure $ case potentialBackendError of
        Left err -> Left (translateBackendError err)
        Right notBackendError -> notBackendError

createMarkov :: MarkovDatabaseBackend m => Text -> MarkovDatabaseMonad a m ()
createMarkov = MTL.lift . backendCreateMarkov

deleteMarkov :: MarkovDatabaseBackend m => Text -> MarkovDatabaseMonad a m ()
deleteMarkov = MTL.lift . backendDeleteMarkov

markovNames :: MarkovDatabaseBackend m => MarkovDatabaseMonad a m [Text]
markovNames = MTL.lift backendMarkovNames

markovExists :: MarkovDatabaseBackend m => Text -> MarkovDatabaseMonad a m Bool
markovExists = MTL.lift . backendMarkovExists

deserializeDB :: (MarkovDatabaseBackend m, Serializable a) => B.ByteString -> MarkovDatabaseMonad a m a
deserializeDB serialized =
    case deserialize serialized of
        Nothing -> MTL.throwError $ CorruptedData serialized
        Just value -> pure value

parseRawMarkovDataEntry :: (MarkovDatabaseBackend m, Serializable a) => (B.ByteString, B.ByteString, Int) -> MarkovDatabaseMonad a m (MarkovToken a, MarkovToken a, Int)
parseRawMarkovDataEntry (seed, value, count) = (,, count) <$> deserializeDB seed <*> deserializeDB value

constructCorpus :: Ord a => [(MarkovToken a, MarkovToken a, Int)] -> Corpus a
constructCorpus =
    let alteration (value, count) existingEntry =
            case existingEntry of
                Nothing -> Just [(value, count)]
                Just entries -> Just $ (value, count) : entries
        
        insertion (seed, value, count) = M.alter (alteration (value, fromIntegral count)) seed
    in Corpus . foldr insertion M.empty

getCorpus :: (MarkovDatabaseBackend m, Serializable a, Ord a) => Text -> MarkovDatabaseMonad a m (Corpus a)
getCorpus markovName = do
    rawMarkovData <- MTL.lift $ backendGetMarkovCounts markovName
    corpusEntries <- traverse parseRawMarkovDataEntry rawMarkovData
    pure $ constructCorpus corpusEntries

serializeMarkovDataEntry :: Serializable a => (MarkovToken a, MarkovToken a) -> (B.ByteString, B.ByteString)
serializeMarkovDataEntry (seed, value) = (serialize seed, serialize value)

processIntoMarkov :: (MarkovDatabaseBackend m, Serializable a, Ord a) => Text -> [[a]] -> MarkovDatabaseMonad a m ()
processIntoMarkov markovName sentences =
    let toSerialize = pairsMultipleSentences sentences
        toPersist = fmap serializeMarkovDataEntry toSerialize
    in MTL.lift $ backendIncrementMarkovCounts markovName toPersist