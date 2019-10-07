{-# LANGUAGE
    RankNTypes,
    TupleSections
#-}

module MarkovDatabase (
    DatabaseError(..),
    MarkovDatabaseMonad,
    hoistBackendAndRun,

    createMarkov,
    deleteMarkov,
    markovNames,

    processIntoMarkov,
    getCorpus
) where

import qualified Control.Monad.Except as MTL
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as M

import Corpus
import MarkovToken
import MarkovDatabaseBackend
import Serializable

data DatabaseError = MarkovNotFound String | CorruptedData B.ByteString
type MarkovDatabaseMonad a m b = MTL.ExceptT DatabaseError m b

hoistBackendAndRun :: (MarkovDatabaseBackend m, Monad n) => (forall x. m x -> n (Either BackendError x)) -> MarkovDatabaseMonad a m b -> n (Either DatabaseError b)
hoistBackendAndRun interpreter action = do
    potentialBackendError <- interpreter (MTL.runExceptT action)
    pure $ case potentialBackendError of
        Left (MarkovNotFoundBackend markovName) -> Left (MarkovNotFound markovName)
        Right notBackendError -> notBackendError

createMarkov :: MarkovDatabaseBackend m => String -> MarkovDatabaseMonad a m ()
createMarkov = MTL.lift . backendCreateMarkov

deleteMarkov :: MarkovDatabaseBackend m => String -> MarkovDatabaseMonad a m ()
deleteMarkov = MTL.lift . backendDeleteMarkov

markovNames :: MarkovDatabaseBackend m => MarkovDatabaseMonad a m [String]
markovNames = MTL.lift backendMarkovNames

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

getCorpus :: (MarkovDatabaseBackend m, Serializable a, Ord a) => String -> MarkovDatabaseMonad a m (Corpus a)
getCorpus markovName = do
    rawMarkovData <- MTL.lift $ backendGetMarkovCounts markovName
    corpusEntries <- traverse parseRawMarkovDataEntry rawMarkovData
    pure $ constructCorpus corpusEntries

serializeMarkovDataEntry :: Serializable a => (MarkovToken a, MarkovToken a) -> (B.ByteString, B.ByteString)
serializeMarkovDataEntry (seed, value) = (serialize seed, serialize value)

processIntoMarkov :: (MarkovDatabaseBackend m, Serializable a, Ord a) => String -> [[a]] -> MarkovDatabaseMonad a m ()
processIntoMarkov markovName sentences =
    let toSerialize = pairsMultipleSentences sentences
        toPersist = fmap serializeMarkovDataEntry toSerialize
    in MTL.lift $ backendIncrementMarkovCounts markovName toPersist