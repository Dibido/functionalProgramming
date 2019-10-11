module DigitalSorting
where
import Data.List (groupBy, sortBy)

class Rank key where
  sort  ::  [(key, val)] -> [val]
  rank  ::  [(key, val)] -> [[val]]
  sort  =  concat . rank

genericSort :: (Ord key) => [(key, val)] -> [val]
genericSort kvs  = map snd (sortBy (\ kv1 kv2 -> compare (fst kv1) (fst kv2)) kvs)

instance Rank () where
  sort kvs   =  map snd kvs
  rank kvs   =  [ map snd kvs | not (null kvs) ]

--6.6.1
newRank :: (Ord key) => [(key,val)] -> [[val]]
newRank kvs = map (\x -> map snd x) (groupBy (\(x1,_) (x2,_) -> x1 == x2) $ sortBy (\(x1,_) (x2,_) -> x1 `compare` x2) kvs)

--6.6.2
instance Rank Int where
  rank = newRank

compare' :: (Eq key, Ord key, Rank key) => key -> key -> Ordering
compare' k1 k2
  | length result == 1 = EQ 
  | (head $ head result) == k1 = LT
  | otherwise = GT
  where result = rank [(k1,k1), (k2,k2)]

test1 = compare' (2::Int) (3::Int)

--6.6.3

instance (Rank key1, Rank key2) => Rank (key1, key2) where

-- instance (Rank key1, Rank key2) => Rank (Either key1 key2) where

type List elem  =  Either () (elem, [elem])

toList :: [elem] -> List elem
toList []        =  Left ()
toList (a : as)  =  Right (a, as)

-- instance (Rank key) => Rank [key] where

-- repeatedSegments :: (Rank key) => Int -> [key] -> [[Integer]]

-- instance Rank Base where
