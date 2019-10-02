module MarkovDatabase (
    MarkovDatabase,

    emptyDatabase,

    makeNewMarkov,
    markovExists,
    deleteMarkov,

    insertIntoMarkov
) where

import Control.Concurrent (MVar, modifyMVar_, newMVar, withMVar)
import qualified Data.Map as M

data MarkovToken a = Begin | Word a | End deriving (Eq, Ord)

type BagInner a = M.Map a Rational
type MarkovInner a = M.Map (MarkovToken a) (BagInner (MarkovToken a))
type DatabaseInner a = M.Map String (MarkovInner a)
newtype MarkovDatabase a = MarkovDatabase (MVar (DatabaseInner a))

emptyDatabase :: IO (MarkovDatabase a)
emptyDatabase = fmap MarkovDatabase (newMVar M.empty)

makeNewMarkov :: MarkovDatabase a -> String -> IO ()
makeNewMarkov (MarkovDatabase mvar) key =
    let markovCreator database = M.insert key M.empty database
    in modifyMVar_ mvar (pure . markovCreator)

markovExists :: MarkovDatabase a -> String -> IO Bool
markovExists (MarkovDatabase mvar) key =
    let markovChecker database = M.member key database
    in withMVar mvar (pure . markovChecker)

deleteMarkov :: MarkovDatabase a -> String -> IO ()
deleteMarkov (MarkovDatabase mvar) key =
    let markovDeleter database = M.delete key database
    in modifyMVar_ mvar (pure . markovDeleter)

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
    let insertPair (key, value) mapping' =
            let alterFunction maybeBag =
                    Just $ case maybeBag of
                        Nothing -> insertIntoBagInner value M.empty
                        Just bag' -> insertIntoBagInner value bag'
            in M.alter alterFunction key mapping'

    in foldr insertPair mapping (tokenPairs sentence)

trainMarkovOnSentences :: Ord a => [[a]] -> MarkovInner a -> MarkovInner a
trainMarkovOnSentences = flip (foldr trainMarkovOnSentence)

insertIntoMarkov :: Ord a => MarkovDatabase a -> String -> [[a]] -> IO ()
insertIntoMarkov = undefined