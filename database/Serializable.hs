{-# LANGUAGE
    FlexibleInstances,
    TypeSynonymInstances
#-}

module Serializable where

import qualified Data.ByteString.Lazy.Char8 as B

class Serializable a where
    serialize :: a -> B.ByteString
    deserialize :: B.ByteString -> Maybe a

instance Serializable String where
    serialize = B.pack
    deserialize = Just . B.unpack