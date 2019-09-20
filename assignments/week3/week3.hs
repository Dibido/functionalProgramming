module Week3 where
import Prelude hiding (Word)
import Data.List
lorem = "Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diamnonumy eirmod tempor invidunt ut labore et dolore magna aliquyamerat, sed diam voluptua. At vero eos et accusam et justo duo doloreset ea rebum. Stet clita kasd gubergren, no sea takimata sanctus estLorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetursadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore etdolore magna aliquyam erat, sed diam voluptua. At vero eos et accusamet justo duo dolores et ea rebum. Stet clita kasd gubergren, no seatakimata sanctus est Lorem ipsum dolor sit amet."
text = "hallo wij zijn aan het programmeren"
--3.1
--type Word = String
--wordList :: String -> [(Word, Int)]
--wordList s = [("ba",2)]
--wordList s = let values = group $ sort $ words $ lorem [((map $ length $ values), (map $ head $ values))
--3.2
--3.3
runs :: (Ord a) => [a] -> [[a]]
runs s = getRuns s [[]]

getRuns :: (Ord a) => [a] -> [[a]] -> [[a]]
getRuns [] result = result
getRuns (x:xs) [[]] = getRuns xs [[x]]
getRuns (x:xs) result
  | x >= (last (last result)) = getRuns xs (init result ++ [(last result) ++ [x]])
  | otherwise = getRuns xs (result ++ [[x]])
-- This function gets all the different runs
--3.4 See DNA.hs
--3.5 See QuickTest.hs
--3.32
data RD    = M | D | C | L | X | V |I
  deriving (Eq, Ord)
data Roman = Roman [RD]

sortedNumerals = [M, D, C, L, X, V, I]

instance Show RD where
  showsPrec _ M = showChar 'M'
  showsPrec _ D = showChar 'D'
  showsPrec _ C = showChar 'C'
  showsPrec _ L = showChar 'L'
  showsPrec _ X = showChar 'X'
  showsPrec _ V = showChar 'V'
  showsPrec _ I = showChar 'I'

roman :: RD -> Int
roman M = 1000
roman D = 500
roman C = 100
roman L = 50
roman X = 10
roman V = 5
roman I = 1

roman2int :: [RD] -> Int
roman2int r = processRomanList (romanRuns (map roman r)) 0

processRomanList :: [[Int]] -> Int -> Int
processRomanList [] sum = sum
processRomanList xs sum
  | length (head xs) == 1 = processRomanList (if length xs >= 2 then tail xs else []) sum+(head (head xs))
  | length (head xs) == 2 = processRomanList (if length xs >= 2 then tail xs else []) sum+(last (head xs) - head (head xs))
  | length (head xs) >= 3 = error "Invalid roman sequence"

romanRuns :: (Ord a) => [a] -> [[a]]
romanRuns s = getRomanRuns s [[]]

getRomanRuns :: (Ord a) => [a] -> [[a]] -> [[a]]
getRomanRuns [] result = result
getRomanRuns (x:xs) [[]] = getRomanRuns xs [[x]]
getRomanRuns (x:xs) result
  | x > (last (last result)) = getRomanRuns xs (init result ++ [(last result) ++ [x]])
  | otherwise = getRomanRuns xs (result ++ [[x]])

int2roman :: Int -> [RD]
int2roman n = getRoman n sortedNumerals []

getRoman :: Int -> [RD] -> [RD] -> [RD]
getRoman n [] result = result
getRoman n [x] result 
  | timesPossible > 0 = replicate timesPossible x ++ result
  | otherwise = result
    where timesPossible = n `quot` roman x
getRoman n (x:xs) result
  | timesPossible > 0 = replicate timesPossible x ++ getRoman (n-(timesPossible * roman x)) xs result 
  | otherwise = getRoman n xs result
    where timesPossible = n `quot` roman x
