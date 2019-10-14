module DigitalSorting
where
import Data.List (groupBy, sortBy)
import List
import DNA hiding (substring)

class Rank key where
  sort  ::  [(key, val)] -> [val]
  rank  ::  [(key, val)] -> [[val]]
  listCompare :: [key]  -> [key] -> Ordering
  compare' :: key -> key -> Ordering
  compare' k1 k2
    | Prelude.length result == 1 = EQ 
    | (head $ head result) == 1 = LT
    | otherwise = GT
      where result = rank [(k1,1), (k2,2)]
  sort  =  concat . rank
  listCompare = listCompare

genericSort :: (Ord key) => [(key, val)] -> [val]
genericSort kvs  = map snd (sortBy (\ kv1 kv2 -> compare (fst kv1) (fst kv2)) kvs)

instance Rank () where
  sort kvs   =  map snd kvs
  listCompare = listCompare
  rank kvs   =  [ map snd kvs | not (null kvs) ]

--6.6.1
newRank :: (Ord key) => [(key,val)] -> [[val]]
newRank kvs = map (\x -> map snd x) (groupBy (\(x1,_) (x2,_) -> x1 == x2) $ sortBy (\(x1,_) (x2,_) -> x1 `compare` x2) kvs)

--6.6.2
instance Rank Int where
  rank = newRank
  listCompare = listCompare

-- See class definition of Rank key
--compare' :: (Eq key, Ord key, Rank key) => key -> key -> Ordering
{-compare' k1 k2
  | length result == 1 = EQ 
  | (head $ head result) == k1 = LT
  | otherwise = GT
  where result = rank [(k1,k1), (k2,k2)]-}

test1 = compare' (2::Int) (3::Int)

--6.6.3

instance (Rank key1, Rank key2) => Rank (key1, key2) where
  rank = rank
  listCompare = listCompare
  compare' (k1, k2) (k3, k4)
    | compare' k1  k3 == LT = LT
    | compare' k1 k3 == GT = GT
    | otherwise = compare' k2 k4

test2 = compare' (1::Int, 2::Int)(1::Int, 3::Int)

--6.6.4

instance (Rank key1, Rank key2) => Rank (Either key1 key2) where
  rank = rank
  listCompare = listCompare
  compare' (Left key1) (Left key2) = compare' key1 key2
  compare' (Left key1) (Right key2) = LT
  compare' (Right key2) (Left key1) = GT
  compare' (Right key1) (Right key2) = compare' key1 key2

--6.6.5

type List elem  =  Either () (elem, [elem])

toList :: [elem] -> List elem
toList []        =  Left ()
toList (a : as)  =  Right (a, as)

instance (Rank key) => Rank [key] where
  rank = rank
  listCompare [] [] = EQ
  listCompare [] (_:_) = LT
  listCompare (_:_) [] = GT
  listCompare (a:as) (b:bs) 
    | compare' a b == EQ = listCompare as bs
    | otherwise = compare' a b

test3 = rank [(3::Int, 6::Int), (3::Int, 4::Int), (3::Int, 9::Int)]

--6.6.6

repeatedSegments :: (Eq key, Rank key) => Int -> [key] -> [[Integer]]
repeatedSegments length list = foldr (\x r -> findSubstring length 0 x [] : r) [] [list]

findSubstring :: (Eq key) => Int -> Int -> [key] -> [Integer] -> [Integer]
findSubstring length index string result = occurrances substr string 0 length [] ++ result
  where substr = substring index length string

occurrances :: (Eq key) => [key] -> [key] -> Int -> Int -> [Integer] -> [Integer]
occurrances substr string index length result
  | index >= DigitalSorting.length string = result
  | substr == cmpstr = (toInteger index) : result ++ (occurrances substr string (index+1) length result)
  | otherwise = result ++ occurrances substr string (index+1) length result
    where cmpstr = substring index length string

substring :: Int -> Int -> [key] -> [key]
substring start end dna = take (end - start) (drop start dna)

length :: [key] -> Int
length list = Prelude.length list

instance Rank Base where
  rank = rank
  listCompare = listCompare
