module Markov (
    Markov,

    emptyMarkov,
    trainMarkovOnSentence,
    trainMarkovOnSentences,

    generateSentence
) where

import Control.Monad.Random (MonadRandom)
import Control.Monad.Reader
import qualified Data.Map as M

import Bag

data MarkovToken a = Begin | Word a | End deriving (Eq, Ord)
newtype Markov a = Markov (M.Map (MarkovToken a) (Bag (MarkovToken a)))

type MarkovMonad a m b = ReaderT (Markov a) m b

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

queryMarkov :: (MonadRandom m, Ord a) => MarkovToken a -> MarkovMonad a m (Maybe (MarkovToken a))
queryMarkov value = do
    Markov mapping <- ask
    case M.lookup value mapping of
        Nothing -> pure Nothing
        Just bag -> lift $ takeWithReplacement bag

generateSentenceWithSeed :: (MonadRandom m, Ord a) => MarkovToken a -> MarkovMonad a m [a]
generateSentenceWithSeed seed =
    let generateSentenceNoPrepend = do
            maybeToken <- queryMarkov seed
            case maybeToken of
                Nothing -> pure []
                Just seed' -> generateSentenceWithSeed seed'

    in case seed of
        Begin -> generateSentenceNoPrepend
        Word a -> fmap (a :) generateSentenceNoPrepend
        End -> pure []

generateSentence :: (MonadRandom m, Ord a) => Markov a -> m [a]
generateSentence = runReaderT $ generateSentenceWithSeed Begin