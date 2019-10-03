{-# LANGUAGE
    DataKinds,
    TypeOperators
#-}

module Api.Markov.ApiType (
    MarkovApi
) where

import Servant.API

import Api.Markov.Models

type GenerateMessageApi = "message" :> Get '[JSON] GeneratedMessage
type TrainingAndCalibrationApi = ReqBody '[JSON] TrainingMessages :> (PostNoContent '[JSON] NoContent :<|> PutNoContent '[JSON] NoContent)
type DeletionApi = DeleteNoContent '[JSON] NoContent

type MarkovApi = GenerateMessageApi :<|> TrainingAndCalibrationApi :<|> DeletionApi