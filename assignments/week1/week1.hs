-- 1.1
-- 1: Returns 6
-- 2: Returns min(m, n)
-- 3: returns the entered text n times
-- 4: performs the mod operator on x and y and then on y (x mod y)
-- 5: adds the first element of the list to the second element
-- 6: inverts the list
-- 7: inverts the list twice
-- 1.2
-- The part between the curly brackets are the ones that are rewritten
-- e1 = {42} = 42
-- e2 = 1 + 125 * 8 / 10 - 59
-- e2 = 1 + 125 * {8 / 10} - 59
-- e2 = 1 + {125 * 0.8} - 59
-- e2 = 1 + {100 - 59}
-- e2 = {1 + 41}
-- e2 = 42
-- e3 = not True || True && False
-- e3 = not True || {True && False}
-- e3 = {not True} || False
-- e3 = {False || False}
-- e3 = False
-- e4 = 1 + 2 == 6 - 3
-- e4 = 1 + 2 == {6 - 3}
-- e4 = {1 + 2} == 3
-- e4 = {3 = 3}
-- e4 = True
-- e5 = "1 + 2" == "6 - 3"
-- e5 = {"1 + 2" == "6 - 3"}
-- e5 = False
-- e6 = "1111 + 2222" == "1111" ++ " + " ++ "2222"
-- e6 = "1111 + 2222" == "1111" ++ {" + " ++ "2222"}
-- e6 = "1111 + 2222" == {"1111" ++ " + 2222"}
-- e6 = {"1111 + 2222" == "1111 + 2222"}
-- e6 = True
-- 1.3
-- See Database.hs
-- 1.4
-- 1.5
-- 1.6
-- 1.7
thisOldMan :: String
thisOldMan = printVerses "" 0

printVerses :: String->Int->String
printVerses s n
  | n >= 10 = s
  | otherwise = printVerse s n ++ printVerses s (n+1)

printVerse :: String->Int->String
printVerse s n = s ++ head poemSentenceNumber ++ head (poemValues !! n) ++ last poemSentenceNumber ++ head poemSentenceThing ++ last (poemValues !! n) ++ last poemSentenceThing ++ poemEnd

poemSentenceNumber :: [String]
poemSentenceNumber = ["This old man, he played ", ",\n"]
poemSentenceThing :: [String]
poemSentenceThing = ["He played knick-knack on my ", ";\n"]

poemEnd :: String
poemEnd = "With a knick-knack paddywhack,\nGive the dog a bone,\nThis old man came rolling home.\n\n"
poemValues :: [[String]]
poemValues = [["one", "thumb"], ["two", "shoe"], ["three", "knee"], ["four", "door"], ["five", "hive"], ["six", "sticks"], ["seven", "heaven"], ["eight", "gate"], ["nine", "spine"], ["ten", "again"]]
-- 1.8
-- See Shapes.hs
-- 1.10
-- See Strings.hs
-- 1.11
isPrime :: Int -> Bool
isPrime n = findPrime n [2..(n-1)]
findPrime :: Int -> [Int] -> Bool
findPrime n [] = if n /= 1 then True else False
findPrime n [x] 
  | isDivisible n x = False
  | otherwise = True
findPrime n (x:xs)
  | isDivisible n x = False
  | otherwise = findPrime n xs
isDivisible n1 n2 = n1 `rem` n2 == 0
-- 1.12 UNFINISHED
primeFactors :: Int -> String
primeFactors n
  | isPrime n = (show n) ++ " is prime"
  | otherwise = findFactors n 0 0 [x | x <- [0..n], isPrime x]

findFactors :: Int -> Int -> Int -> [Int] -> String
findFactors n1 i1 i2 ns
  | (ns !! i1) * (ns !! i2) == n1 = (show (ns !! i1)) ++ "*" ++ (show (ns !! i2))
  | i2 == (length ns)-1 = findFactors n1 (i1+1) 0 ns
  | i1 == (length ns)-1 = "unable to find factors"
  | otherwise = findFactors n1 i1 (i2+1) ns
-- 1.13
numberSum :: Int -> Int
numberSum n = sumNumber 0 n 0

sumNumber :: Int -> Int -> Int -> Int
sumNumber counter n index
  | index >= length (show n) = counter
  | otherwise = addNumber counter n index + sumNumber counter n (index+1)
  where addNumber counter number index = counter + read ([show (number) !! index]) :: Int
-- 1.14
-- Triangle
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
-- 1.15
g = 9.81
v_x t v0 theta0 = v0 * cos theta0
v_y t v0 theta0 = v0 * sin theta0 - g * t
x_at t v0 theta0 = v0 * cos theta0 * t
y_at t v0 theta0 = v0 * sin theta0 * t - (1/2) * g * t**2
h x v0 theta0 = tan theta0 * x - (g / 2*(v0 * cos theta0)) * x**2

highestPoint v0 theta0 = ((v0 * sin theta0) / g) 
maximalHeight v0 theta0 = y_at (highestPoint v0 theta0)
getDistance v0 theta0 = x_at (2 * (highestPoint v0 theta0)) v0 theta0

best_angle v0 = findBestAngle v0  0
findBestAngle v0 theta0 
  | getDistance v0 theta0 > getDistance v0 (theta0+(1/100*pi)) = theta0
  | otherwise = findBestAngle v0 (theta0+(1/100*pi))
