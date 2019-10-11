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
import Control.Monad.Catch (catch)
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
            . CQL.setLogger (CQL.stdoutLogger CQL.LogDebug)
            . CQL.setContacts host []
            $ CQL.defSettings
    in CQL.init settings

newtype CassandraBackend a = CassandraBackend (MTL.ExceptT BackendError CQL.Client a)
    deriving (Functor, Applicative, Monad, MTL.MonadError BackendError, MTL.MonadIO)

runCassandraBackend :: CQL.ClientState -> CassandraBackend a -> IO (Either BackendError a)
runCassandraBackend state (CassandraBackend action) = CQL.runClient state . MTL.runExceptT $ action

liftClient :: CQL.Client a -> CassandraBackend a
liftClient action =
    let exceptionHandler :: CQL.ResponseError -> CQL.Client (Either CQL.ResponseError a)
        exceptionHandler = pure . Left
    in do
        result <- CassandraBackend . MTL.lift $ fmap Right action `catch` exceptionHandler
        case result of
            Left err -> MTL.throwError $ OtherError (show err)
            Right result' -> pure result'

createMarkovQuery :: CQL.PrepQuery CQL.W (CQL.Identity Text.Text) ()
createMarkovQuery = "INSERT INTO markov_names (markov_name) VALUES (?) IF NOT EXISTS"

deleteMarkovQuery :: CQL.PrepQuery CQL.W (CQL.Identity Text.Text) ()
deleteMarkovQuery = "DELETE FROM markov_names WHERE markov_name = ?"

deleteMarkovEntriesQuery :: CQL.PrepQuery CQL.W (CQL.Identity Text.Text) ()
deleteMarkovEntriesQuery = "DELETE FROM markov_data where markov_name = ?"

existsMarkovQuery :: CQL.PrepQuery CQL.R (CQL.Identity Text.Text) (CQL.Identity Text.Text)
existsMarkovQuery = "SELECT markov_name FROM markov_names WHERE markov_name = ?"

listMarkovNamesQuery :: CQL.PrepQuery CQL.R () (CQL.Identity Text.Text)
listMarkovNamesQuery = "SELECT markov_name FROM markov_names"

getCountsQuery :: CQL.PrepQuery CQL.R (CQL.Identity Text.Text) (CQL.Blob, CQL.Blob, CQL.Counter)
getCountsQuery = "SELECT seed, value, count FROM markov_data where markov_name = ?"

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

    backendDeleteMarkov markovName = liftClient $ do
        CQL.write deleteMarkovQuery (markovNameParam markovName)
        CQL.write deleteMarkovEntriesQuery (markovNameParam markovName)

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
            liftClient . CQL.batch $
                CQL.setType CQL.BatchUnLogged
                *> traverse_ (CQL.addPrepQuery insertMarkovDataQuery) params