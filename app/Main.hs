module Main where

import System.Environment (getArgs)

import Markov

main :: IO ()
main = do
    [trainFile] <- getArgs
    sentences <- fmap (fmap words . lines) $ readFile trainFile

    let markov = trainMarkovOnSentences sentences emptyMarkov
    generatedSentence <- generateSentence markov
    putStrLn $ unwords generatedSentence