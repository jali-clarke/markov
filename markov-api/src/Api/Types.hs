{-# LANGUAGE
    TypeOperators
#-}

module Api.Types where

import Data.Proxy
import Servant.API

import Api.MarkovMaps.Types
import Api.MarkovMaps.Message.Types

type Api = MarkovMapsApi :<|> MessageApi

api :: Proxy Api
api = Proxy