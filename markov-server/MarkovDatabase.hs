module MarkovDatabase (
    MarkovDatabase,
    emptyDatabase,
    markovNames,

    MarkovDatabaseMonad,
    DatabaseError(..),
    runMarkovDatabaseMonad,

    makeNewMarkov,
    deleteMarkov,
    insertIntoMarkov,

    getCorpus
) where

import Control.Concurrent (MVar, modifyMVar_, newMVar, withMVar)
import Control.Monad (when)
import qualified Control.Monad.Except as MTL
import qualified Control.Monad.Reader as MTL
import qualified Data.Map as M

import Corpus
import MarkovToken

type BagInner a = M.Map a Rational
type MarkovInner a = M.Map (MarkovToken a) (BagInner (MarkovToken a))
type DatabaseInner a = M.Map String (MarkovInner a)
newtype MarkovDatabase a = MarkovDatabase (MVar (DatabaseInner a))

data DatabaseError = MarkovNotFound String
type MarkovDatabaseMonad a b = MTL.ReaderT (MarkovDatabase a) (MTL.ExceptT DatabaseError IO) b

runMarkovDatabaseMonad :: MarkovDatabaseMonad a b -> MarkovDatabase a -> IO (Either DatabaseError b)
runMarkovDatabaseMonad action database = MTL.runExceptT $ MTL.runReaderT action database

emptyDatabase :: IO (MarkovDatabase a)
emptyDatabase = fmap MarkovDatabase (newMVar M.empty)

modifyDatabase :: (DatabaseInner a -> DatabaseInner a) -> MarkovDatabaseMonad a ()
modifyDatabase modifier = do
    MarkovDatabase mvar <- MTL.ask
    MTL.liftIO $ modifyMVar_ mvar (pure . modifier)

withDatabase :: (DatabaseInner a -> b) -> MarkovDatabaseMonad a b
withDatabase accessor = do
    MarkovDatabase mvar <- MTL.ask
    MTL.liftIO $ withMVar mvar (pure . accessor)

markovNames :: MarkovDatabaseMonad a [String]
markovNames = withDatabase M.keys

makeNewMarkov :: String -> MarkovDatabaseMonad a ()
makeNewMarkov key =
    let markovCreator database = M.insert key M.empty database
    in modifyDatabase markovCreator

markovExists :: String -> MarkovDatabaseMonad a Bool
markovExists key =
    let markovChecker database = M.member key database
    in withDatabase markovChecker

modifyMarkov :: String -> (MarkovInner a -> MarkovInner a) -> MarkovDatabaseMonad a ()
modifyMarkov markovName modifier =
    let alterFunction = fmap modifier
        databaseModifier database = M.alter alterFunction markovName database
    in do
        markovNameExists <- markovExists markovName
        when (not markovNameExists) (MTL.throwError $ MarkovNotFound markovName)
        modifyDatabase databaseModifier

withMarkov :: String -> (MarkovInner a -> b) -> MarkovDatabaseMonad a b
withMarkov markovName accessor =
    let databaseAccessor database = fmap accessor $ M.lookup markovName database
    in do
        result <- withDatabase databaseAccessor
        case result of
            Nothing -> MTL.throwError $ MarkovNotFound markovName
            Just result' -> pure result'

deleteMarkov :: String -> MarkovDatabaseMonad a ()
deleteMarkov key =
    let markovDeleter database = M.delete key database
    in modifyDatabase markovDeleter

insertIntoBagInner :: Ord a => a -> BagInner a -> BagInner a
insertIntoBagInner value mapping =
    let alterFunction count =
            case count of
                Nothing -> Just 1
                Just count' -> Just (count' + 1)
    in M.alter alterFunction value mapping

tokenPairs :: [a] -> [(MarkovToken a, MarkovToken a)]
tokenPairs xs =
    let pairsHelper tokensList =
            case tokensList of
                [] -> []
                [x] -> [(x, End)]
                x : rest@(y : _) -> (x, y) : pairsHelper rest
    in pairsHelper (Begin : fmap Word xs)

trainMarkovOnSentence :: Ord a => [a] -> MarkovInner a -> MarkovInner a
trainMarkovOnSentence sentence mapping =
    let alterFunction value maybeBag =
            Just $ case maybeBag of
                Nothing -> insertIntoBagInner value M.empty
                Just bag -> insertIntoBagInner value bag

        insertPair (key, value) mapping' = M.alter (alterFunction value) key mapping'
    in foldr insertPair mapping (tokenPairs sentence)

trainMarkovOnSentences :: Ord a => [[a]] -> MarkovInner a -> MarkovInner a
trainMarkovOnSentences = flip (foldr trainMarkovOnSentence)

insertIntoMarkov :: Ord a => String -> [[a]] -> MarkovDatabaseMonad a ()
insertIntoMarkov markovName sentences = modifyMarkov markovName (trainMarkovOnSentences sentences)

getCorpus :: Ord a => String -> MarkovDatabaseMonad a (Corpus a)
getCorpus markovName =
    let createCorpus = Corpus . M.map M.toList
    in withMarkov markovName createCorpus