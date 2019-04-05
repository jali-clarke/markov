module Markov (
    buildMarkov,
    getNext,
    stream
) where

import Data.Maybe (maybe)
import qualified Data.Map as Map
import Control.Monad.Random.Lazy (MonadRandom)

import Probable

newtype Markov a = Markov (Map.Map a (Probable a))

buildMarkov :: Ord a => [a] -> Markov a
buildMarkov = buildMarkovHelper . calculateStats

getNext :: (Ord a, MonadRandom m) => Markov a -> a -> m (Maybe a)
getNext (Markov markovMap) a = maybe (pure Nothing) measure (Map.lookup a markovMap)

stream :: (Ord a, MonadRandom m) => Markov a -> a -> m [a]
stream markov seed = getNext markov seed >>= fmap (seed :) . (maybe (pure []) (stream markov))

buildMarkovHelper :: Map.Map a (Map.Map a Int) -> Markov a
buildMarkovHelper =
    let
        mapToProbable = buildProbable . fmap (\(a, count) -> (fromIntegral count, a)) . Map.assocs
    in Markov . Map.map mapToProbable

calculateStats :: Ord a => [a] -> Map.Map a (Map.Map a Int)
calculateStats = foldr statsHelper Map.empty . pairs

pairs :: [a] -> [(a, a)]
pairs as =
    case as of
        [a, a'] -> [(a, a')]
        a : a' : as' -> (a, a') : pairs (a' : as')
        _ -> error "can't do pairs on list that does not have at least two elements"

statsHelper :: Ord a => (a, a) -> Map.Map a (Map.Map a Int) -> Map.Map a (Map.Map a Int)
statsHelper (first, next) statsMap =
    let
        innerStatsHelper = maybe (Just 1) (Just . (+ 1))
        outerStatsHelper = Just . Map.alter innerStatsHelper next . maybe (Map.empty) id
    in Map.alter outerStatsHelper first statsMap