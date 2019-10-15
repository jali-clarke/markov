module Main where

import Api.MarkovMaps.Handlers
import Api.MarkovMaps.Types

import ServiceMain

main :: IO ()
main = serviceMain "markov-crud" markovMapsApi markovMapsHandler