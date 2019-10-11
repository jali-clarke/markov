{-# LANGUAGE
    GeneralizedNewtypeDeriving,
    OverloadedStrings
#-}

module CassandraBackend (
    clientInitState,

    CassandraBackend,
    runCassandraBackend
) where

import Control.Monad (unless)
import qualified Control.Monad.Except as MTL
import Data.Foldable (traverse_)
import Data.Functor (void)
import qualified Data.Text as Text
import qualified Database.CQL.IO as CQL
import qualified Database.CQL.Protocol as CQL

import MarkovDatabaseBackend

clientInitState :: String -> IO CQL.ClientState
clientInitState host =
    let settings =
            CQL.setKeyspace (CQL.Keyspace "markov")
            . CQL.setLogger (CQL.stdoutLogger CQL.LogInfo)
            . CQL.setContacts host []
            $ CQL.defSettings
    in CQL.init settings

newtype CassandraBackend a = CassandraBackend (MTL.ExceptT BackendError CQL.Client a)
    deriving (Functor, Applicative, Monad, MTL.MonadError BackendError, MTL.MonadIO)

runCassandraBackend :: CQL.ClientState -> CassandraBackend a -> IO (Either BackendError a)
runCassandraBackend state (CassandraBackend action) = CQL.runClient state . MTL.runExceptT $ action

liftClient :: CQL.Client a -> CassandraBackend a
liftClient = CassandraBackend . MTL.lift

createMarkovQuery :: CQL.PrepQuery CQL.W (CQL.Identity Text.Text) ()
createMarkovQuery = "INSERT INTO markov_names (markov_name) VALUES (?) IF NOT EXISTS"

deleteMarkovQuery :: CQL.PrepQuery CQL.W (CQL.Identity Text.Text) ()
deleteMarkovQuery = "DELETE FROM markov_names WHERE markov_name = ?"

deleteMarkovEntriesQuery :: CQL.PrepQuery CQL.W (CQL.Identity Text.Text) ()
deleteMarkovEntriesQuery = "DELETE FROM markov_data where markov_name = ?"

existsMarkovQuery :: CQL.PrepQuery CQL.R (CQL.Identity Text.Text) (CQL.Identity Text.Text)
existsMarkovQuery = "SELECT markov_name FROM markov_names WHERE markov_name = ?"

listMarkovNamesQuery :: CQL.PrepQuery CQL.R () (CQL.Identity Text.Text)
listMarkovNamesQuery = "SELECT table_name FROM system_schema.tables where keyspace_name = 'markov'"

getCountsQuery :: CQL.PrepQuery CQL.R (CQL.Identity Text.Text) (CQL.Blob, CQL.Blob, CQL.Counter)
getCountsQuery = "SELECT seed, value, count FROM ?"

insertMarkovDataQuery :: CQL.PrepQuery CQL.W (Text.Text, CQL.Blob, CQL.Blob) ()
insertMarkovDataQuery = "UPDATE markov_names SET count = count + 1 WHERE markov_name = ? AND seed = ? AND value = ?"

queryParams :: a -> CQL.QueryParams a
queryParams = CQL.defQueryParams CQL.Quorum

markovNameParam' :: String -> CQL.Identity Text.Text
markovNameParam' = CQL.Identity . Text.pack

markovNameParam :: String -> CQL.QueryParams (CQL.Identity Text.Text)
markovNameParam = queryParams . markovNameParam'

checkMarkovExists :: String -> CassandraBackend ()
checkMarkovExists markovName = do
    exists <- liftClient . fmap (/= Nothing) . CQL.query1 existsMarkovQuery . markovNameParam $ markovName
    unless exists (MTL.throwError $ MarkovNotFoundBackend markovName)

instance MarkovDatabaseBackend CassandraBackend where
    backendMarkovNames =
        let toString (CQL.Identity text) = Text.unpack text
        in liftClient . fmap (fmap toString) $ CQL.query listMarkovNamesQuery (queryParams ())

    backendCreateMarkov = liftClient . void . CQL.write createMarkovQuery . markovNameParam

    backendDeleteMarkov markovName = liftClient . CQL.batch $ do
        CQL.addPrepQuery deleteMarkovQuery (markovNameParam' markovName)
        CQL.addPrepQuery deleteMarkovEntriesQuery (markovNameParam' markovName)

    backendGetMarkovCounts markovName =
        let convertRow (CQL.Blob seed, CQL.Blob value, CQL.Counter count) = (seed, value, fromIntegral count)
        in do
            checkMarkovExists markovName
            liftClient . fmap (fmap convertRow) . CQL.query getCountsQuery . markovNameParam $ markovName

    backendIncrementMarkovCounts markovName entries =
        let packedMarkovName = Text.pack markovName
            params = fmap (\(seed, value) -> (packedMarkovName, CQL.Blob seed, CQL.Blob value)) entries
        in do
            checkMarkovExists markovName
            liftClient . CQL.batch $ traverse_ (CQL.addPrepQuery insertMarkovDataQuery) params