module Main where

import Servant

import Api.HandlerHelpers
import Api.MarkovMaps.Message.Handlers
import Api.MarkovMaps.Message.Types
import CassandraBackend

import ServiceMain

app :: String -> IO Application
app dbHost = do
    clientState <- clientInitState dbHost
    let interpreter = hoistToHandler (runCassandraBackend clientState)
    pure $ serve messageApi $ hoistServer messageApi interpreter generateMessageHandler

main :: IO ()
main = serviceMain "markov-sentence-generator" app