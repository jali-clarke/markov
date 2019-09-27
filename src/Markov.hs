module Markov (
    Markov,

    emptyMarkov,
    trainMarkovOnSentence,
    trainMarkovOnSentences,

    generateSentence
) where

import Control.Monad.Random (MonadRandom)
import qualified Data.Map as M

import Bag

data Markov a = Markov (Bag a) (M.Map a (Bag a))

emptyMarkov :: Markov a
emptyMarkov = Markov emptyBag M.empty

trainMarkovOnSentence :: Ord a => [a] -> Markov a -> Markov a
trainMarkovOnSentence sentence markov@(Markov bag mapping) =
    let pairs list =
            case list of
                [] -> []
                _ : [] -> []
                x : rest@(y : _) -> (x, y) : pairs rest

        insertPair (key, value) mapping' =
            let alterFunction maybeBag =
                    Just $ case maybeBag of
                        Nothing -> insert value emptyBag
                        Just bag' -> insert value bag'
            in M.alter alterFunction key mapping'

    in case sentence of
        [] -> markov
        word : _ ->
            let newBag = insert word bag
                newMapping = foldr insertPair mapping (pairs sentence)
            in Markov newBag newMapping

trainMarkovOnSentences :: Ord a => [[a]] -> Markov a -> Markov a
trainMarkovOnSentences = flip (foldr trainMarkovOnSentence)

queryMarkov :: (MonadRandom m, Ord a) => Markov a -> a -> m (Maybe a)
queryMarkov (Markov _ mapping) value =
    case M.lookup value mapping of
        Nothing -> pure Nothing
        Just bag -> takeWithReplacement bag

getMarkovInit :: MonadRandom m => Markov a -> m (Maybe a)
getMarkovInit (Markov bag _) = takeWithReplacement bag

generateSentence :: (MonadRandom m, Ord a) => Markov a -> m [a]
generateSentence markov =
    let generateSentenceWith action = do
            token <- action
            case token of
                Nothing -> pure []
                Just token' -> fmap (token' :) (generateSentenceHelper token')

        generateSentenceHelper token = generateSentenceWith (queryMarkov markov token)
    in generateSentenceWith (getMarkovInit markov)