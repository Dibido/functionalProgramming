triangle :: Int -> String
triangle n = getTriangleLine "" 0 n

getTriangleLine :: String -> Int -> Int -> String
getTriangleLine s n1 n2
  | n2 == 0 = s
  | otherwise = s ++ getSpaces "" (n2-1) ++ getStars "" (n1+1) ++ "\n" ++ getTriangleLine s (n1+1) (n2-1)

getSpaces :: String -> Int -> String
getSpaces s n 
  | n == 0 = s
  | otherwise = s ++ " " ++ getSpaces s (n-1)

getStars :: String -> Int -> String
getStars s n
  | n == 0 = s
  | n == 1      = s ++ "*" ++ getStars s (n-1)
  | otherwise   = s ++ "*" ++ getStars s (n-1) ++ "*"

--triangle 5
-- Result = "     ∗\n   ∗ ∗ ∗\n  ∗ ∗ ∗ ∗ ∗\n ∗ ∗ ∗ ∗ ∗ ∗ ∗\n∗ ∗ ∗ ∗ ∗ ∗ ∗ ∗ ∗\n"
