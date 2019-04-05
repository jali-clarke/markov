module Tokens (
    SentenceToken(..),

    tokenize,
    detokenize
) where

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
