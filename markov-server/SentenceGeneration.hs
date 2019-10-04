module SentenceGeneration (
    generateSentence
) where

import Control.Monad.Random (MonadRandom)

import Corpus
import MarkovToken

generateSentenceWithSeed :: (MonadRandom m, Ord a) => MarkovToken a -> Corpus a -> m [a]
generateSentenceWithSeed seed markov =
    let generateSentenceNoPrepend seed' markov' = do
            maybeToken <- queryCorpus seed' markov'
            case maybeToken of
                Nothing -> pure []
                Just seed'' -> generateSentenceWithSeed seed'' markov'
    in case seed of
        Begin -> generateSentenceNoPrepend seed markov
        Word a -> fmap (a :) (generateSentenceNoPrepend seed markov)
        End -> pure []

generateSentence :: (MonadRandom m, Ord a) => Corpus a -> m [a]
generateSentence = generateSentenceWithSeed Begin