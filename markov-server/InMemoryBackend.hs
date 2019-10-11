{-# LANGUAGE
    GeneralizedNewtypeDeriving,
    TupleSections
#-}

module InMemoryBackend (
    InMemoryDB,
    emptyInMemoryDB,

    InMemoryBackend,
    runWithInMemoryDB
) where

import Control.Concurrent (MVar, modifyMVar, modifyMVar_, newMVar, withMVar)
import Control.Monad (unless)
import qualified Control.Monad.Except as MTL
import qualified Control.Monad.Reader as MTL
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as M

import MarkovDatabaseBackend

type BagInner = M.Map B.ByteString Int
type MarkovInner = M.Map B.ByteString BagInner
type DatabaseInner = M.Map String MarkovInner
type InMemoryDB = MVar DatabaseInner

emptyInMemoryDB :: IO InMemoryDB
emptyInMemoryDB = newMVar M.empty

newtype InMemoryBackend a = InMemoryBackend (MTL.ReaderT InMemoryDB (MTL.ExceptT BackendError IO) a)
    deriving (Functor, Applicative, Monad, MTL.MonadReader InMemoryDB, MTL.MonadError BackendError, MTL.MonadIO)

runWithInMemoryDB :: InMemoryDB -> InMemoryBackend a -> IO (Either BackendError a)
runWithInMemoryDB db (InMemoryBackend action) = MTL.runExceptT $ MTL.runReaderT action db

withActionOnDatabase :: (InMemoryDB -> (DatabaseInner -> IO a) -> IO b) -> (DatabaseInner -> a) -> InMemoryBackend b
withActionOnDatabase mvarAction databaseFunction = do
    mvar <- MTL.ask
    MTL.liftIO $ mvarAction mvar (pure . databaseFunction)

withDatabase :: (DatabaseInner -> a) -> InMemoryBackend a
withDatabase = withActionOnDatabase withMVar

modifyDatabase :: (DatabaseInner -> DatabaseInner) -> InMemoryBackend ()
modifyDatabase = withActionOnDatabase modifyMVar_

modifyDatabaseWithReturn :: (DatabaseInner -> (a, DatabaseInner)) -> InMemoryBackend a
modifyDatabaseWithReturn modifier =
    let swap (a, b) = (b, a)
    in withActionOnDatabase modifyMVar (swap . modifier)

withMarkov :: String -> (MarkovInner -> a) -> InMemoryBackend a
withMarkov markovName accessor = do
    maybeResult <- withDatabase (fmap accessor . M.lookup markovName)
    maybe (MTL.throwError $ MarkovNotFoundBackend markovName) pure maybeResult

modifyMarkov :: String -> (MarkovInner -> MarkovInner) -> InMemoryBackend ()
modifyMarkov markovName modifier =
    let databaseAlteration = maybe (False, Nothing) ((True,) . Just . modifier)
    in do
        succeeded <- modifyDatabaseWithReturn $ M.alterF databaseAlteration markovName
        unless succeeded (MTL.throwError $ MarkovNotFoundBackend markovName)

flattenBag :: (B.ByteString, BagInner) -> [(B.ByteString, B.ByteString, Int)]
flattenBag (seed, bag) = fmap (\(value, count) -> (seed, value, count)) (M.toList bag)

flatten :: MarkovInner -> [(B.ByteString, B.ByteString, Int)]
flatten = foldr (++) [] . fmap flattenBag . M.toList

incrementCountInBag :: B.ByteString -> BagInner -> BagInner
incrementCountInBag =
    let alteration = maybe (Just 1) (Just . (+ 1))
    in M.alter alteration

incrementCount :: (B.ByteString, B.ByteString) -> MarkovInner -> MarkovInner
incrementCount (seed, value) =
    let alteration = Just . incrementCountInBag value . maybe M.empty id
    in M.alter alteration seed

instance MarkovDatabaseBackend InMemoryBackend where
    backendCreateMarkov =
        let alteration = Just . maybe M.empty id
        in modifyDatabase . M.alter alteration

    backendDeleteMarkov = modifyDatabase . M.delete
    backendMarkovNames = withDatabase M.keys

    backendGetMarkovCounts markovName = withMarkov markovName flatten
    backendIncrementMarkovCounts markovName = modifyMarkov markovName . flip (foldr incrementCount)