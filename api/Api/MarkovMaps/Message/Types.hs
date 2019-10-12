{-# LANGUAGE
    DataKinds,
    DeriveGeneric
#-}

module Api.MarkovMaps.Message.Types where

import Data.Aeson
import Data.Proxy
import GHC.Generics
import Servant.API

data GeneratedMessage = GeneratedMessage {message :: String} deriving Generic
instance ToJSON GeneratedMessage

type MessageApi = Get '[JSON] GeneratedMessage

messageApi :: Proxy MessageApi
messageApi = Proxy