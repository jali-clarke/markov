module Probable (
    Probable,

    measure,
    buildProbable
) where

import Control.Monad.Random.Lazy (MonadRandom(getRandomR))

data Probable a =
    Nil
    | Leaf Double a
    | Node Int Double (Probable a) Double (Probable a)
    deriving Show

measureHelper :: Probable a -> Double -> Maybe a
measureHelper probable p =
    case probable of
        Nil -> Nothing
        Leaf _ a -> Just a
        Node _ pLeft probLeft pRight probRight ->
            if p > pLeft
                then measureHelper probRight (p - pLeft)
                else measureHelper probLeft p

measure :: MonadRandom m => Probable a -> m (Maybe a)
measure probable = fmap (measureHelper probable) $ getRandomR (0, 1)

numLeaves :: Probable a -> Int
numLeaves probable =
    case probable of
        Nil -> 0
        Leaf _ _ -> 1
        Node n _ _ _ _ -> n

buildHelper :: (Double, a) -> Probable a -> Probable a
buildHelper (p, a) probable =
    case probable of
        Nil -> Leaf p a
        Leaf p' _ -> Node 2 p' probable p (Leaf p a)
        Node n pLeft probLeft pRight probRight ->
            if numLeaves probLeft < numLeaves probRight
                then
                    let
                        newProbLeft = buildHelper (p, a) probLeft
                    in Node (n + 1) (pLeft + p) newProbLeft pRight probRight
                else
                    let
                        newProbRight = buildHelper (p, a) probRight
                    in Node (n + 1) pLeft probLeft (pRight + p) newProbRight

buildProbable :: [(Double, a)] -> Probable a
buildProbable probabilities =
    let
        allowedProbabilities = filter (\(p, _) -> p > 0) probabilities
        totalProbability = sum . fmap fst $ allowedProbabilities
        normalized = fmap (\(p, a) -> (p / totalProbability, a)) allowedProbabilities
    in foldr buildHelper Nil normalized