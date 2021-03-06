module MarkovToken (
    MarkovToken(..),

    pairs,
    pairsMultipleSentences
) where

import qualified Data.ByteString.Lazy as B

import Serializable

data MarkovToken a = Begin | Word a | End deriving (Eq, Ord)

instance Serializable a => Serializable (MarkovToken a) where
    serialize token =
        case token of
            Begin -> B.singleton 0
            Word a -> B.cons 1 (serialize a)
            End -> B.singleton 2

    deserialize bytes = do
        (tag, rest) <- B.uncons bytes
        case tag of
            0 -> if B.null rest then pure Begin else Nothing
            1 -> fmap Word $ deserialize rest
            2 -> if B.null rest then pure End else Nothing
            _ -> Nothing

pairs :: [a] -> [(MarkovToken a, MarkovToken a)]
pairs sentence =
    let pairsHelper sentence' =
            case sentence' of
                [] -> []
                [word] -> [(word, End)]
                word0 : rest@(word1 : _) -> (word0, word1) : pairsHelper rest
    in pairsHelper (Begin : fmap Word sentence)

pairsMultipleSentences :: [[a]] -> [(MarkovToken a, MarkovToken a)]
pairsMultipleSentences = foldr (++) [] . fmap pairs