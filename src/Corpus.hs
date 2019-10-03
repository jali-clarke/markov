module Corpus (
    Corpus(..),
    queryCorpus
) where

import Control.Monad.Random (MonadRandom, fromListMay)
import qualified Data.Map as M

import MarkovToken

type Bag a = [(MarkovToken a, Rational)]
newtype Corpus a = Corpus (M.Map (MarkovToken a) (Bag a))

bagFor :: Ord a => MarkovToken a -> Corpus a -> Maybe (Bag a)
bagFor seed (Corpus mapping) = M.lookup seed mapping

takeWithReplacement :: MonadRandom m => Bag a -> m (Maybe (MarkovToken a))
takeWithReplacement bag = fromListMay bag

queryCorpus :: (MonadRandom m, Ord a) => MarkovToken a -> Corpus a -> m (Maybe (MarkovToken a))
queryCorpus value corpus = do
    case bagFor value corpus of
        Nothing -> pure Nothing
        Just bag -> takeWithReplacement bag