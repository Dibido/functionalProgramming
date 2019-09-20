-- Assignments Week 3 Dibran Dokter s1047390 & Marnix Lukasse s1047400
--3.5 See QuickTest.hs
--3.32
data RD    = M | CM | D | CD | C | L | XL | X | IX | V | IV |I
  deriving (Eq, Ord)
data Roman = Roman [RD]

sortedNumerals = [M, CM, D, CD, C, L, XL, X, IX, V, IV, I]

instance Show RD where
  showsPrec _ M   = showChar 'M'
  showsPrec _ CM  = showString "CM"
  showsPrec _ D   = showChar 'D'
  showsPrec _ CD  = showString "CD"
  showsPrec _ C   = showChar 'C'
  showsPrec _ L   = showChar 'L'
  showsPrec _ XL  = showString "XL"
  showsPrec _ X   = showChar 'X'
  showsPrec _ IX  = showString "IX"
  showsPrec _ V   = showChar 'V'
  showsPrec _ IV  = showString "IV"
  showsPrec _ I   = showChar 'I'

roman :: RD -> Int
roman M   = 1000
roman CM  = 900
roman D   = 500
roman CD  = 400
roman C   = 100
roman L   = 50
roman XL  = 40
roman X   = 10
roman IX  = 9
roman V   = 5
roman IV  = 4
roman I   = 1

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
