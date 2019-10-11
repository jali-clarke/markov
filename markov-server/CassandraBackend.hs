module CassandraBackend (
    clientInitState,

    CassandraBackend,
    runCassandraBackend
) where

import qualified Control.Monad.Except as MTL
import qualified Database.CQL.IO as CQL

import MarkovDatabaseBackend

clientInitState :: String -> IO CQL.ClientState
clientInitState host =
    let settings = CQL.setLogger (CQL.stdoutLogger CQL.LogInfo) . CQL.setContacts host [] $ CQL.defSettings
    in CQL.init settings

newtype CassandraBackend a = CassandraBackend (MTL.ExceptT BackendError CQL.Client a)

runCassandraBackend :: CQL.ClientState -> CassandraBackend a -> IO (Either BackendError a)
runCassandraBackend state (CassandraBackend action) = CQL.runClient state . MTL.runExceptT $ action