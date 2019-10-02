{-# LANGUAGE
    DataKinds,
    DeriveGeneric,
    TypeOperators
#-}

module Api (
    api,
    Api,

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

type GenerateMessageApi = "message" :> Get '[JSON] GeneratedMessage
type TrainingAndCalibrationApi = ReqBody '[JSON] TrainingMessages :> (PostNoContent '[JSON] NoContent :<|> PutNoContent '[JSON] NoContent)
type DeletionApi = DeleteNoContent '[JSON] NoContent

type MarkovApi = GenerateMessageApi :<|> TrainingAndCalibrationApi :<|> DeletionApi
type Api = Capture "markovName" String :> MarkovApi

api :: Proxy Api
api = Proxy