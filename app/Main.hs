module Main where

import Network.Wai.Handler.Warp
import Servant
import System.Environment (getArgs)
import Text.Read (readMaybe)

import Api
import MarkovDatabase

main :: IO ()
main = do
    args <- getArgs
    case args of
        [portString] ->
            case readMaybe portString of
                Nothing -> putStrLn "invalid port"
                Just port -> do
                    markov <- emptyDatabase
                    let application = serve api (apiServer markov)
                    run port application
        _ -> putStrLn "usage: markov <port>"