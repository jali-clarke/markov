module Serializable where

import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

class Serializable a where
    serialize :: a -> B.ByteString
    deserialize :: B.ByteString -> Maybe a

instance Serializable Text.Text where
    serialize = B.toLazyByteString . Text.encodeUtf8Builder
    deserialize = Just . Text.decodeUtf8 . B.toStrict