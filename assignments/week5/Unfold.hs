module Unfold
where

import Prelude hiding (take, (++))
import qualified Data.List as L
import Data.Either
import Data.Maybe

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
       Right(a, new_seed) -> a : produce new_seed

take :: Int -> [a] -> [a]
take n l = unfoldr (\s -> if (null (fst s)) || (snd s <= 0) then Nothing else Just ((head $ fst s), (tail $ fst s, (snd s)-1))) (l, n)

filter :: (a -> Bool) -> [a] -> [a]
filter pred l = unfoldr (\x -> case x of [] -> Nothing; (x:xs) -> if pred x then Just (x, xs) else Nothing) l 

fibs :: [Integer]
fibs = unfoldr (\(a,b) -> Just (a, (b,a+b))) (0,1)

notMultipleOf :: Integer -> Integer -> Bool
notMultipleOf n1 n2 = (n2 `mod` n1) /= 0

primes :: [Integer]
primes = unfoldr (\(p:xs) -> Just (p, (Prelude.filter (notMultipleOf p) xs))) [2..]

unfoldr' :: (t -> Maybe (a, t)) -> t -> [a]
unfoldr' rep seed = apo (\t -> Right (fromJust (rep t))) seed

fibs' = unfoldr' (\(a,b) -> Just (a, (b,a+b))) (0,1)

(++) :: [a] -> [a] -> [a]-- Defined in ‘GHC.Base’
infixr 5 ++
(++) l1 l2 = apo (\s -> if null s then Left l2 else Right (head s, tail s)) l1

insert' :: (Ord a) => a -> [a] -> [a]
insert' x l = apo (\s -> if null s then Left s else if head s >= x then Left (x : s) else Right (head s, tail s) ) l
