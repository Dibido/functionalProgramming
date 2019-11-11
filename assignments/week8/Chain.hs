module Chain
where

import Satellite

-- Costs and dimensions.

type Cost  =  Integer
type Dim   =  (Integer, Integer)

(<**>) :: Dim -> Dim -> With Cost Dim
(i, j) <**> (j', k)
  | j == j'    =  (i * j * k) :- (i, k)
  | otherwise  =  error "<**>: dimensions do not match"

(<***>) :: With Cost Dim -> With Cost Dim -> With Cost Dim
(c1 :- d1) <***> (c2 :- d2)
  =  (c1 + c + c2) :- d where c :- d =  d1 <**> d2

-- Minimal costs.

minCost :: [Dim] -> With Cost Dim
minCost [a]  =  0 :- a
minCost as   =  minimum [ minCost bs <***> minCost cs | (bs, cs) <- split as ]

split :: [a] -> [([a], [a])]
split []        =  error "split: empty list"
split [_a]      =  []
split (a : as)  =  ([a], as) : [ (a : bs, cs) | (bs, cs) <- split as]

minTest1 = minCost [(10, 30), (30, 5), (5, 60)]
minTest2 = minCost [ (i, i + 1) | i <- [1 .. 3] ]
minTest3 = minCost [ (i, i + 1) | i <- [1 .. 9] ]

minimumCost   :: (size -> size -> With Cost size) -> [size] -> With Cost size
minimumCost f [a] = 0 :- a
minimumCost f as  = minimum [compareLists (c1 :- s1) (c2 :- s2) | (c1 :- s1, c2 :- s2) <- [((minimumCost f bs), (minimumCost f cs)) | (bs,cs) <- split as ]]
  where compareLists (c1 :- s1) (c2 :- s2) = (c1 + c + c2) :- s where c :- s = f s1 s2

testMinimumCost1 = minimumCost (<**>) [(10, 30), (30, 5), (5, 60)]
testMinimumCost2 = minimumCost (<**>) [ (i, i + 1) | i <- [1..3] ]
testMinimumCost3 = minimumCost (<**>) [ (i, i + 1) | i <- [1..9] ]

concatCost :: Integer -> Integer -> With Cost Integer
concatCost a b = a :- (a + b)

addCost :: Integer -> Integer -> With Cost Integer
addCost a b = max a b :- a + b

-- Optimal chain.

optimalChain  :: (size -> size -> With Cost size) -> [size] -> With Cost (With size (Tree size))
optimalChain f [a] = 0 :- (a :- (Leaf a))
--optimalChain f as = 
