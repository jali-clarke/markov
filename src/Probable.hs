module Probable (
    Probable,

    measure,
    buildProbable
) where

import Control.Monad.Random.Lazy (MonadRandom(getRandomR))

data Probable a =
    Nil
    | Leaf Double a
    | Node Int Double (Probable a) (Probable a)

measureHelper :: Probable a -> Double -> Maybe a
measureHelper probable p =
    case probable of
        Nil -> Nothing
        Leaf _ a -> Just a
        Node _ pLeft probLeft probRight ->
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
        Node n _ _ _  -> n

buildHelper :: (Double, a) -> Probable a -> Probable a
buildHelper (p, a) probable =
    case probable of
        Nil -> Leaf p a
        Leaf p' _ -> Node 2 p' probable (Leaf p a)
        Node n pLeft probLeft probRight ->
            if numLeaves probLeft < numLeaves probRight
                then Node (n + 1) (pLeft + p) (buildHelper (p, a) probLeft) probRight
                else Node (n + 1) pLeft probLeft (buildHelper (p, a) probRight)

buildProbable :: [(Double, a)] -> Probable a
buildProbable probabilities =
    let
        allowedProbabilities = filter (\(p, _) -> p > 0) probabilities
        totalProbability = sum . fmap fst $ allowedProbabilities
        normalized = fmap (\(p, a) -> (p / totalProbability, a)) allowedProbabilities
    in foldr buildHelper Nil normalized