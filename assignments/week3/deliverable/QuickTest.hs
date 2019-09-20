module QuickTest (Probes, Property, (?->), (?=>))
where
import Data.List (sort)

type Probes a    =  [a]

type Property a  =  a -> Bool

infixr 1  ?->, ?=>

(?->)   :: Probes a -> Property b -> Property (a -> b)
(?=>)   :: Probes a -> (a -> Property b) -> Property (a -> b)

probes ?-> prop  =  \ f -> and [ prop (f x)   | x <- probes ]
probes ?=> prop  =  \ f -> and [ prop x (f x) | x <- probes ]

--3.5.1
ordered      :: (Ord a) => Property [a]
ordered [x] = True
ordered (x:xs) = x <= head xs && ordered xs 

--3.5.2
permutations :: [a] -> Probes [a]
permutations [] = [[]]
permutations (x:xs) =
  foldr (++) [] (map (interleave [] x) (permutations xs))
    where
      interleave :: [a] -> a -> [a] -> [[a]]
      interleave xs x [] = [xs ++ [x]]
      interleave xs x (y:ys) =
        (xs ++ (x:y:ys)) : 
          (interleave (xs ++ [y]) x ys)

--3.5.3
runs :: (Ord a) => [a] -> [[a]]
runs s = getRuns s [[]]

getRuns :: (Ord a) => [a] -> [[a]] -> [[a]]
getRuns [] result = result
getRuns (x:xs) [[]] = getRuns xs [[x]]
getRuns (x:xs) result
  | x >= (last (last result)) = getRuns xs (init result ++ [(last result) ++ [x]])
  | otherwise = getRuns xs (result ++ [[x]])

-- Test procedure for runs
testRuns = (permutations ['a'..'f'] ?-> all ordered) runs

--3.5.4
isqrt :: Integer -> Integer
isqrt n = loop 0 3 1
  where loop i k s  | s <= n      = loop (i + 1) (k + 2) (s + k)
                    | otherwise  = i

-- Test procedure for isqrt
testSqrt = ([0..100] ?=> \inp res -> intSqrt inp == res) isqrt

intSqrt :: Integer -> Integer
intSqrt n = toInteger (floor (sqrt (fromIntegral n)))

--3.5.5
-- Due to problems with ambigious occurence of <*> we decided to use ***
infixr 4 ***
(***) :: Probes a -> Probes b -> Probes (a, b)
[] *** b = []
a *** [] = []
a *** b = combinations (head a) b ++ ((tail a) *** b)

-- This results in n * m tuples.

combinations :: a -> Probes b -> Probes (a,b)
combinations a [] = []
combinations a b = [(a, head b)] ++ combinations a (tail b)
 
niftySort :: [a] -> [a]
niftySort _xs  =  []

trustedSort :: (Ord a) => [a] -> [a]
trustedSort  =  sort
