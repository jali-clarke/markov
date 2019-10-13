module Main where

import Servant

import Api.HandlerHelpers
import Api.MarkovMaps.Handlers
import Api.MarkovMaps.Types
import CassandraBackend

import ServiceMain

app :: String -> IO Application
app dbHost = do
    clientState <- clientInitState dbHost
    let interpreter = hoistToHandler (runCassandraBackend clientState)
    pure $ serve markovMapsApi $ hoistServer markovMapsApi interpreter markovMapsHandler

main :: IO ()
main = serviceMain "markov-crud" app