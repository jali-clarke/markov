module Corpus (
    Bag,
    Corpus(..),

    bagFor
) where

import qualified Data.Map as M

import MarkovToken

type Bag a = [(MarkovToken a, Rational)]
newtype Corpus a = Corpus (M.Map (MarkovToken a) (Bag a))

bagFor :: Ord a => MarkovToken a -> Corpus a -> Maybe (Bag a)
bagFor seed (Corpus mapping) = M.lookup seed mapping