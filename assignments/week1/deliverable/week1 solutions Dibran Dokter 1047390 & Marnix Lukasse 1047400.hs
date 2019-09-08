-- Assignments Week 1 Dibran Dokter s1047390 & Marnix Lukasse s1047400
--1.8
module Shapes
where

data Shape
  =  Circle Double            -- radius
  |  Square Double            -- length
  |  Rectangle Double Double  -- length and width
  deriving (Show)

showShape :: Shape -> String
showShape (Circle r)       =  "circle of radius " ++ show r
showShape (Square l)       =  "square of length " ++ show l
showShape (Rectangle l w)  =  "rectangle of length " ++ show l
                                ++ " and width " ++ show w
area        :: Shape -> Double
area (Circle r) = pi * r**2
area (Square l) = l * l
area (Rectangle l w) = l * w

perimeter   :: Shape -> Double
perimeter (Circle r) = 2 * (pi * r)
perimeter (Square l) = 4 * l
perimeter (Rectangle l w) = 2 * (l + w)

center       :: Shape -> (Double, Double)  -- x- and y-coordinates
center (Circle r) = (r, r)
center (Square l) = (l/2, l/2)
center (Rectangle l w) = (l/2, w/2)

boundingBox  :: Shape -> (Double, Double)  -- width and height
boundingBox (Circle r) = (r*2, r*2)
boundingBox (Square l) = (l, l)
boundingBox (Rectangle l w)
  | l > w = (l,l)
  | w > l = (w,w)
--1.14
-- For this problem we both made very different solutions so we have listed them both here, for the next problems we will pair program to avoid this.

-- SOLUTION 1: Marnix Lukasse
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

-- SOLUTION 2: Dibran Dokter
triangle :: Int -> String
triangle n = getTriangleLine "" 0 n n

-- Make an entire line of the triangle and recursively add lines to for the triangle
getTriangleLine :: String -> Int -> Int -> Int -> String
getTriangleLine s stars spaces triangles
  | triangles == 0 = s
  | otherwise = s ++ getSpaces "" (spaces-1) ++ getStars "" (stars+1) ++ "\n" ++ getTriangleLine s (stars+1) (spaces-1) (triangles-1)

-- Get the number of spaces required before the stars
getSpaces :: String -> Int -> String
getSpaces s n 
  | n == 0 = s
  | otherwise = s ++ " " ++ getSpaces s (n-1)

-- Get the number of stars, first only one star then n stars with a star at either side
getStars :: String -> Int -> String
getStars s n
  | n == 0 = s
  | n == 1      = s ++ "*" ++ getStars s (n-1)
  | otherwise   = s ++ "*" ++ getStars s (n-1) ++ "*"

--Christmas tree
christmasTree :: Int -> String
christmasTree n = makeTree "" 0 n

-- Make a tree by adding extra spaces to the triangles to make them line up
makeTree :: String -> Int -> Int -> String
makeTree s n1 n2 
  | n1 == n2  = s ++ triangle n2
  | otherwise = s ++ getTriangleLine "" 0 (n2) n1 ++ makeTree s (n1+1) n2
