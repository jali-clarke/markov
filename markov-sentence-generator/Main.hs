module Main where

import Api.MarkovMaps.Message.Handlers
import Api.MarkovMaps.Message.Types

import ServiceMain

main :: IO ()
main = serviceMain "markov-sentence-generator" messageApi generateMessageHandler