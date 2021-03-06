{-# LANGUAGE
    DataKinds,
    DeriveGeneric,
    OverloadedStrings,
    TypeOperators
#-}

module Api.MarkovMaps.Types where

import Data.Aeson
import Data.Proxy
import Data.Text (Text)
import GHC.Generics
import Servant.API

import Api.ModelHelpers

data MarkovMap = MarkovMap {name :: Text, href :: Maybe Text} deriving Generic
instance ToJSON MarkovMap
instance FromJSON MarkovMap

markovMapWithHref :: Text -> MarkovMap
markovMapWithHref markovName = MarkovMap markovName (Just $ "/markovMaps/" <> markovName)

data TrainingMessages = TrainingMessages {messages :: [Text]} deriving Generic
instance FromJSON TrainingMessages

type MarkovMapGetAll = Get '[JSON] (ListOf MarkovMap)
type MarkovMapCreate = ReqBody '[JSON] MarkovMap :> PostNoContent '[JSON] NoContent
type MarkovMapsManyApi = MarkovMapGetAll :<|> MarkovMapCreate

type MarkovMapGetOne = Get '[JSON] MarkovMap
type MarkovMapTrain = ReqBody '[JSON] TrainingMessages :> PutNoContent '[JSON] NoContent
type MarkovMapDelete = DeleteNoContent '[JSON] NoContent
type MarkovMapsOneApi = MarkovMapGetOne :<|> MarkovMapTrain :<|> MarkovMapDelete

type MarkovMapsApi = "markovMaps" :> (MarkovMapsManyApi :<|> (Capture "markovMap" Text :> MarkovMapsOneApi))

markovMapsApi :: Proxy MarkovMapsApi
markovMapsApi = Proxy