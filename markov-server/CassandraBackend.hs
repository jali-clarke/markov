{-# LANGUAGE
    GeneralizedNewtypeDeriving,
    OverloadedStrings
#-}

module CassandraBackend (
    clientInitState,

    CassandraBackend,
    runCassandraBackend
) where

import qualified Control.Monad.Except as MTL
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

queryParams :: a -> CQL.QueryParams a
queryParams = CQL.defQueryParams CQL.Quorum

markovNameParam' :: String -> CQL.Identity Text.Text
markovNameParam' = CQL.Identity . Text.pack

markovNameParam :: String -> CQL.QueryParams (CQL.Identity Text.Text)
markovNameParam = queryParams . markovNameParam'

-- NEED TO CHECK WHETHER MARKOV MAP EXISTS BEFORE WRITING / READING

instance MarkovDatabaseBackend CassandraBackend where
    backendMarkovNames =
        let toString (CQL.Identity text) = Text.unpack text
        in liftClient . fmap (fmap toString) $ CQL.query listMarkovNamesQuery (queryParams ())

    backendCreateMarkov = liftClient . void . CQL.write createMarkovQuery . markovNameParam

    backendDeleteMarkov markovName = liftClient . CQL.batch $ do
        CQL.addPrepQuery deleteMarkovQuery (markovNameParam' markovName)
        CQL.addPrepQuery deleteMarkovEntriesQuery (markovNameParam' markovName)

    backendGetMarkovCounts =
        let convertRow (CQL.Blob seed, CQL.Blob value, CQL.Counter count) = (seed, value, fromIntegral count)
        in liftClient . fmap (fmap convertRow) . CQL.query getCountsQuery . markovNameParam