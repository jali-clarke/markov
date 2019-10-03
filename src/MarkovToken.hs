module MarkovToken (
    MarkovToken(..)
) where

data MarkovToken a = Begin | Word a | End deriving (Eq, Ord)