getSpaces :: Int ->  String
getSpaces n
  | n < 1 = ""
  | n >= 1 = " " ++ getSpaces(n-1)

getStars :: Int ->  String
getStars n
  | n == 0 = ""
  | n >= 1 = "*" ++ getStars(n-1)

christmasTree :: Int -> IO ()
christmasTree nTrees = putStr (makeTreeString nTrees (nTrees * 2 - 1))
  
makeTreeString :: Int -> Int -> String  
makeTreeString nTrees nLongestRow
  | nTrees <= 0 = ""
  | otherwise = makeTreeString (nTrees - 1) nLongestRow ++ triangle nTrees nLongestRow
  
triangle :: Int -> Int -> String
triangle n nLongestRow = makeRows 0 n nLongestRow
  
-- Makerows makes multiple rows, starting with 1 centered star and adds 2 stars on both sides each line until theres a full line of stars
makeRows :: Int -> Int -> Int -> String
makeRows currentRow totalRows nLongestRow
  | currentRow >= totalRows = ""
  | otherwise = makeRow nStars nLongestRow ++ "\n" ++ makeRows (currentRow + 1) totalRows nLongestRow
  where nStars = 1 + 2 * currentRow  
        nRowSize = 2 * totalRows - 1
  
-- Makerow makes a row with uneven amount of symbols filled with spaces and *'s, with the *s being centered
makeRow :: Int -> Int -> String
makeRow nStars nRowSize
  | nRowSize `mod` 2 == 0 = "" -- Can't do even number of symbols
  | nStars `mod` 2 == 0 = "" -- Can't do even number of stars
  | nStars <= 0 = getSpaces(nRowSize)
  | nStars == nRowSize = getStars(nRowSize)
  | otherwise = getSpaces(nSpaces) ++ getStars(nStars) ++ getSpaces(nSpaces)
  where nSpaces = (nRowSize - nStars) `div` 2