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

data MarkovToken a = Begin | Word a | End deriving (Eq, Ord)
newtype Markov a = Markov (M.Map (MarkovToken a) (Bag (MarkovToken a)))

emptyMarkov :: Markov a
emptyMarkov = Markov M.empty

tokenPairs :: [a] -> [(MarkovToken a, MarkovToken a)]
tokenPairs xs =
    let pairsHelper tokensList =
            case tokensList of
                [] -> []
                [x] -> [(x, End)]
                x : rest@(y : _) -> (x, y) : pairsHelper rest
    in pairsHelper (Begin : fmap Word xs)

trainMarkovOnSentence :: Ord a => [a] -> Markov a -> Markov a
trainMarkovOnSentence sentence (Markov mapping) =
    let insertPair (key, value) mapping' =
            let alterFunction maybeBag =
                    Just $ case maybeBag of
                        Nothing -> insert value emptyBag
                        Just bag' -> insert value bag'
            in M.alter alterFunction key mapping'

    in Markov $ foldr insertPair mapping (tokenPairs sentence)

trainMarkovOnSentences :: Ord a => [[a]] -> Markov a -> Markov a
trainMarkovOnSentences = flip (foldr trainMarkovOnSentence)

queryMarkov :: (MonadRandom m, Ord a) => MarkovToken a -> Markov a -> m (Maybe (MarkovToken a))
queryMarkov value (Markov mapping) =
    case M.lookup value mapping of
        Nothing -> pure Nothing
        Just bag -> takeWithReplacement bag

generateSentenceWithSeed :: (MonadRandom m, Ord a) => MarkovToken a -> Markov a -> m [a]
generateSentenceWithSeed seed markov =
    let generateSentenceNoPrepend = do
            maybeToken <- queryMarkov seed markov
            case maybeToken of
                Nothing -> pure []
                Just seed' -> generateSentenceWithSeed seed' markov

    in case seed of
        Begin -> generateSentenceNoPrepend
        Word a -> fmap (a :) generateSentenceNoPrepend
        End -> pure []

generateSentence :: (MonadRandom m, Ord a) => Markov a -> m [a]
generateSentence = generateSentenceWithSeed Begin