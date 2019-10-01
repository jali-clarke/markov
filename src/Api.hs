{-# LANGUAGE
    DataKinds,
    DeriveGeneric,
    TypeOperators
#-}

module Api (
    api,
    Api,

    GenerateMessageApi,
    TrainApi,
    CalibrateApi,

    GeneratedMessage(..),
    TrainingMessages(..)
) where

import Data.Aeson
import Data.Proxy
import GHC.Generics
import Servant.API

data GeneratedMessage = GeneratedMessage {message :: String} deriving Generic
instance ToJSON GeneratedMessage

data TrainingMessages = TrainingMessages {messages :: [String]} deriving Generic
instance FromJSON TrainingMessages

type GenerateMessageApi = "generateMessage" :> Get '[JSON] GeneratedMessage
type TrainApi = "train" :> ReqBody '[JSON] TrainingMessages :> PostNoContent '[JSON] NoContent
type CalibrateApi = "calibrate" :> ReqBody '[JSON] TrainingMessages :> PostNoContent '[JSON] NoContent

type Api = GenerateMessageApi :<|> TrainApi :<|> CalibrateApi

api :: Proxy Api
api = Proxy