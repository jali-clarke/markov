{-# LANGUAGE
    FlexibleContexts,
    GeneralizedNewtypeDeriving,
    OverloadedStrings
#-}

module CassandraBackend (
    clientInitState,

    CassandraBackend,
    runCassandraBackend
) where

import Control.Monad.Catch (catch)
import qualified Control.Monad.Except as MTL
import Data.Foldable (traverse_)
import Data.Functor (void)
import qualified Data.Text as Text
import Data.UUID (UUID)
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
liftClient action =
    let exceptionHandler :: CQL.ResponseError -> CQL.Client (Either CQL.ResponseError a)
        exceptionHandler = pure . Left
    in do
        result <- CassandraBackend . MTL.lift $ fmap Right action `catch` exceptionHandler
        case result of
            Left err -> MTL.throwError $ OtherError (show err)
            Right result' -> pure result'

createMarkovQuery :: CQL.PrepQuery CQL.W (CQL.Identity Text.Text) CQL.Row
createMarkovQuery = "INSERT INTO markov_names (markov_name, markov_id) VALUES (?, uuid()) IF NOT EXISTS"

deleteMarkovQuery :: CQL.PrepQuery CQL.W (CQL.Identity Text.Text) ()
deleteMarkovQuery = "DELETE FROM markov_names WHERE markov_name = ?"

deleteMarkovEntriesQuery :: CQL.PrepQuery CQL.W (CQL.Identity UUID) ()
deleteMarkovEntriesQuery = "DELETE FROM markov_data where markov_id = ?"

markovIdQuery :: CQL.PrepQuery CQL.R (CQL.Identity Text.Text) (CQL.Identity UUID)
markovIdQuery = "SELECT markov_id FROM markov_names WHERE markov_name = ?"

listMarkovNamesQuery :: CQL.PrepQuery CQL.R () (CQL.Identity Text.Text)
listMarkovNamesQuery = "SELECT markov_name FROM markov_names"

getCountsQuery :: CQL.PrepQuery CQL.R (CQL.Identity UUID) (CQL.Blob, CQL.Blob, CQL.Counter)
getCountsQuery = "SELECT seed, value, count FROM markov_data where markov_id = ?"

insertMarkovDataQuery :: CQL.PrepQuery CQL.W (UUID, CQL.Blob, CQL.Blob) ()
insertMarkovDataQuery = "UPDATE markov_data SET count = count + 1 WHERE markov_id = ? AND seed = ? AND value = ?"

queryParams :: a -> CQL.QueryParams a
queryParams = CQL.defQueryParams CQL.Quorum

param :: a -> CQL.QueryParams (CQL.Identity a)
param = queryParams . CQL.Identity

markovId :: Text.Text -> CassandraBackend UUID
markovId markovName = do
    uuid <- liftClient . CQL.query1 markovIdQuery . param $ markovName
    maybe (MTL.throwError $ MarkovNotFoundBackend markovName) (pure . CQL.runIdentity) uuid

instance MarkovDatabaseBackend CassandraBackend where
    backendMarkovNames = liftClient . fmap (fmap CQL.runIdentity) $ CQL.query listMarkovNamesQuery (queryParams ())

    backendCreateMarkov = liftClient . void . CQL.trans createMarkovQuery . param

    backendDeleteMarkov markovName =
        let handleError err =
                case err of
                    MarkovNotFoundBackend _ -> pure ()
                    _ -> MTL.throwError err
        in flip MTL.catchError handleError $ do
            uuid <- markovId markovName
            liftClient $ do
                CQL.write deleteMarkovQuery (param markovName)
                CQL.write deleteMarkovEntriesQuery (param uuid)

    backendMarkovExists markovName = (True <$ markovId markovName) `MTL.catchError` (\_ -> pure False)

    backendGetMarkovCounts markovName =
        let convertRow (CQL.Blob seed, CQL.Blob value, CQL.Counter count) = (seed, value, fromIntegral count)
        in do
            uuid <- markovId markovName
            liftClient . fmap (fmap convertRow) . CQL.query getCountsQuery . param $ uuid

    backendIncrementMarkovCounts markovName entries = do
        uuid <- markovId markovName
        let params = fmap (\(seed, value) -> (uuid, CQL.Blob seed, CQL.Blob value)) entries
        liftClient . CQL.batch $
            CQL.setType CQL.BatchUnLogged
            *> traverse_ (CQL.addPrepQuery insertMarkovDataQuery) params