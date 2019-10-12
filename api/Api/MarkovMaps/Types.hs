{-# LANGUAGE
    DataKinds,
    DeriveGeneric,
    TypeOperators
#-}

module Api.MarkovMaps.Types where

import Data.Aeson
import Data.Proxy
import Data.Text (Text, unpack)
import GHC.Generics
import Servant.API

import Api.ModelHelpers
import Api.MarkovMaps.Message.Types

data MarkovMap = MarkovMap {name :: Text, href :: Maybe String} deriving Generic
instance ToJSON MarkovMap
instance FromJSON MarkovMap

markovMapWithHref :: Text -> MarkovMap
markovMapWithHref markovName = MarkovMap markovName (Just $ "/markovMaps/" ++ unpack markovName)

data TrainingMessages = TrainingMessages {messages :: [String]} deriving Generic
instance FromJSON TrainingMessages

type MarkovMapGetAll = Get '[JSON] (ListOf MarkovMap)
type MarkovMapCreate = ReqBody '[JSON] MarkovMap :> PostNoContent '[JSON] NoContent
type MarkovMapsManyApi = MarkovMapGetAll :<|> MarkovMapCreate

type MarkovMapGetOne = Get '[JSON] MarkovMap
type MarkovMapTrain = ReqBody '[JSON] TrainingMessages :> PutNoContent '[JSON] NoContent
type MarkovMapDelete = DeleteNoContent '[JSON] NoContent
type MarkovMapsOneApi = (MarkovMapGetOne :<|> MarkovMapTrain :<|> MarkovMapDelete) :<|> ("message" :> MessageApi)

type MarkovMapsApi = MarkovMapsManyApi :<|> (Capture "markovMap" Text :> MarkovMapsOneApi)

markovMapApi :: Proxy MarkovMapsApi
markovMapApi = Proxy