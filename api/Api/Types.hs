{-# LANGUAGE
    DataKinds,
    TypeOperators
#-}

module Api.Types (
    Api,
    api
) where

import Data.Proxy
import Servant.API

import Api.MarkovMaps.Types

type Api = "markovMaps" :> MarkovMapsApi

api :: Proxy Api
api = Proxy