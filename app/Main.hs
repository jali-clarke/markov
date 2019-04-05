module Main where

import Markov

data SentenceToken =
    Begin
    | Word String
    | End
    deriving (Show, Eq, Ord)

safeTail :: [a] -> [a]
safeTail [] = []
safeTail (_ : as) = as

isTerminus :: String -> Bool
isTerminus word = any (== last word) ['.', '?', '!']

tokenizeHelper :: String -> [SentenceToken]
tokenizeHelper str =
    case str of
        [] -> []
        _ ->
            let (word, rest) = span (/= ' ') str
                safeRest = safeTail rest
                wordToken = Word word
            in if isTerminus word then wordToken : End : tokenize safeRest else wordToken : tokenizeHelper safeRest

tokenize :: String -> [SentenceToken]
tokenize str =
    case str of
        [] -> []
        _ -> Begin : tokenizeHelper str

detokenize' :: [SentenceToken] -> (String -> String) -> String
detokenize' tokens acc =
    case tokens of
        [] -> acc ""
        Begin : Word word : rest -> detokenize' rest ((++ word) . acc)
        Begin : End : rest -> detokenize' rest acc
        [Begin] -> acc ""
        Word word : rest -> detokenize' rest ((++ (' ' : word)) . acc)
        End : rest'@(Begin : rest) -> detokenize' rest' ((++ " ") . acc)
        [End] -> acc ""

detokenize :: [SentenceToken] -> String
detokenize = flip detokenize' id

string = "There are many variations of passages of Lorem Ipsum available, but the majority have suffered alteration in some form, by injected humour, or randomised words which don't look even slightly believable. If you are going to use a passage of Lorem Ipsum, you need to be sure there isn't anything embarrassing hidden in the middle of text. All the Lorem Ipsum generators on the Internet tend to repeat predefined chunks as necessary, making this the first true generator on the Internet. It uses a dictionary of over 200 Latin words, combined with a handful of model sentence structures, to generate Lorem Ipsum which looks reasonable. The generated Lorem Ipsum is therefore always free from repetition, injected humour, or non-characteristic words etc."

main :: IO ()
main =
    let
        tokens = tokenize string
        markov = buildMarkov tokens
        tokenStream = stream markov Begin
        sentence = fmap (detokenize . take 100) tokenStream
    in sentence >>= putStrLn