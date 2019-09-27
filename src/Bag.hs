module Bag (
    Bag,

    emptyBag,
    insert,
    takeWithReplacement
) where

import Control.Monad.Random (MonadRandom, fromListMay)
import qualified Data.Map as M

newtype Bag a = Bag (M.Map a Rational)

emptyBag :: Bag a
emptyBag = Bag M.empty

insert :: Ord a => a -> Bag a -> Bag a
insert value (Bag mapping) =
    let alterFunction count =
            case count of
                Nothing -> Just 1
                Just count' -> Just (count' + 1)
    in Bag $ M.alter alterFunction value mapping

takeWithReplacement :: MonadRandom m => Bag a -> m (Maybe a)
takeWithReplacement (Bag mapping) = fromListMay (M.toList mapping)