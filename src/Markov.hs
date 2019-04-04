module Markov (
    calculateStats,
    pairs
) where

import qualified Data.Map as Map
import Probable

newtype Markov a = Markov (Map.Map a (Probable a))

calculateStats :: Ord a => [a] -> Map.Map a (Map.Map a Int)
calculateStats as = foldr statsHelper Map.empty (pairs as)

pairs :: [a] -> [(a, a)]
pairs as =
    case as of
        [a, a'] -> [(a, a')]
        a : a' : as' -> (a, a') : pairs (a' : as')
        _ -> error "can't do pairs on list that does not have at least two elements"

statsHelper :: Ord a => (a, a) -> Map.Map a (Map.Map a Int) -> Map.Map a (Map.Map a Int)
statsHelper (first, next) statsMap =
    let
        innerStatsHelper value =
            case value of
                Nothing -> Just 1
                Just count -> Just (count + 1)

        outerStatsHelper innerStatsMapMaybe =
            let
                innerStatsMap' =
                    case innerStatsMapMaybe of
                        Nothing -> Map.empty
                        Just innerStatsMap -> innerStatsMap
            in Just $ Map.alter innerStatsHelper next innerStatsMap'
    in Map.alter outerStatsHelper first statsMap