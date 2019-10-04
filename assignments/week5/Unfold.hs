module Unfold
where

import Prelude hiding (take, filter)
import qualified Data.List as L

unfoldr :: (t -> Maybe (a, t)) -> t -> [a]
unfoldr rep seed = produce seed
  where
    produce seed = case rep seed of 
       Just (a, new_seed) -> a : produce new_seed
       Nothing            -> []

apo :: (t -> Either [a] (a, t)) -> t -> [a]
apo rep seed = produce seed
  where
    produce seed = case rep seed of 
       Left l     -> l
       Right(a,ns) -> a : produce ns

take :: Int -> [a] -> [a]
take n l = unfoldr (\s -> if (null (fst s)) || (snd s <= 0) then Nothing else Just ((head $ fst s), (tail $ fst s, (snd s)-1))) (l, n)

filter :: (a -> Bool) -> [a] -> [a]
filter pred l = unfoldr (\x -> case x of [] -> Nothing; x:xs -> if pred x then Just (x, xs) else Nothing) l
