module SentenceGeneration (
    generateSentence
) where

import Control.Monad.Random (MonadRandom, fromListMay)
import qualified Data.Map as M

import Corpus
import MarkovToken

takeWithReplacement :: MonadRandom m => Bag a -> m (Maybe (MarkovToken a))
takeWithReplacement bag = fromListMay bag

queryMarkov :: (MonadRandom m, Ord a) => MarkovToken a -> Corpus a -> m (Maybe (MarkovToken a))
queryMarkov value corpus = do
    case bagFor value corpus of
        Nothing -> pure Nothing
        Just bag -> takeWithReplacement bag

generateSentenceWithSeed ::(MonadRandom m, Ord a) => MarkovToken a -> Corpus a -> m [a]
generateSentenceWithSeed seed markov =
    let generateSentenceNoPrepend seed' markov' = do
            maybeToken <- queryMarkov seed' markov'
            case maybeToken of
                Nothing -> pure []
                Just seed'' -> generateSentenceWithSeed seed'' markov'
    in case seed of
        Begin -> generateSentenceNoPrepend seed markov
        Word a -> fmap (a :) (generateSentenceNoPrepend seed markov)
        End -> pure []

generateSentence :: (MonadRandom m, Ord a) => Corpus a -> m [a]
generateSentence = generateSentenceWithSeed Begin