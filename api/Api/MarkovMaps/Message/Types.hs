{-# LANGUAGE
    DataKinds,
    DeriveGeneric,
    TypeOperators
#-}

module Api.MarkovMaps.Message.Types where

import Data.Aeson
import Data.Proxy
import Data.Text (Text)
import GHC.Generics
import Servant.API

data GeneratedMessage = GeneratedMessage {message :: String} deriving Generic
instance ToJSON GeneratedMessage

type MessageApi = "markovMaps" :> Capture "markovMap" Text :> "message" :> Get '[JSON] GeneratedMessage

messageApi :: Proxy MessageApi
messageApi = Proxy