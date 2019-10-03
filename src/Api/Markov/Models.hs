{-# LANGUAGE
    DeriveGeneric
#-}

module Api.Markov.Models (
    GeneratedMessage(..),
    TrainingMessages(..)
) where

import Data.Aeson
import GHC.Generics

data GeneratedMessage = GeneratedMessage {message :: String} deriving Generic
instance ToJSON GeneratedMessage

data TrainingMessages = TrainingMessages {messages :: [String]} deriving Generic
instance FromJSON TrainingMessages
