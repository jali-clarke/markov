module Markov (
    Markov,

    trainMarkovOnSentence,
    trainMarkovOnSentences,

    generateSentence
) where

import Control.Monad.Random (MonadRandom)

data Markov a

emptyMarkov :: Markov a
emptyMarkov = undefined

trainMarkovOnSentence :: Ord a => [a] -> Markov a -> Markov a
trainMarkovOnSentence = undefined

trainMarkovOnSentences :: Ord a => [[a]] -> Markov a -> Markov a
trainMarkovOnSentences = flip (foldr trainMarkovOnSentence)

queryMarkov :: MonadRandom m => Markov a -> a -> m (Maybe a)
queryMarkov = undefined

getMarkovInit :: MonadRandom m => Markov a -> m (Maybe a)
getMarkovInit = undefined

generateSentence :: MonadRandom m => Markov a -> m [a]
generateSentence markov =
    let generateSentenceWith action = do
            token <- action
            case token of
                Nothing -> pure []
                Just token' -> fmap (token' :) (generateSentenceHelper token')

        generateSentenceHelper token = generateSentenceWith (queryMarkov markov token)
    in generateSentenceWith (getMarkovInit markov)