module MarkovToken (
    MarkovToken(..)
) where

import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as B

import Serializable

data MarkovToken a = Begin | Word a | End deriving (Eq, Ord)

instance Serializable a => Serializable (MarkovToken a) where
    serialize token =
        case token of
            Begin -> B.toLazyByteString $ B.word8 0
            Word a -> B.toLazyByteString $ B.word8 1 <> B.lazyByteString (serialize a)
            End -> B.toLazyByteString $ B.word8 2

    deserialize bytes = do
        (tag, rest) <- B.uncons bytes
        case tag of
            0 -> if B.null rest then pure Begin else Nothing
            1 -> fmap Word $ deserialize rest
            2 -> if B.null rest then pure End else Nothing
            _ -> Nothing