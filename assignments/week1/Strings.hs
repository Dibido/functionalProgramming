module Strings 
where

head :: String -> Char
head s = s !! 0

tail :: String -> String
tail (x:xs) = xs

is_equal :: String -> String -> Bool
is_equal s1 s2 = s1 == s2

is_substring :: String -> String -> Bool
is_substring s1 s2 = subString s1 s2 0

subString :: String -> String -> Int -> Bool
subString s1 s2 index 
  | index > length s2 = False
  | otherwise = getSubString index (index+length(s1)) s2 == s1 || subString s1 s2 (index+1)

getSubString :: Int -> Int -> String -> String
getSubString start end text = take (end - start) (drop start text)

is_sub :: String -> String -> Bool
is_sub s1 s2 = findChars s1 s2 0 0

findChars :: String -> String -> Int -> Int -> Bool
findChars s1 s2 index1 index2 
  | index1 == length s1-1 && equalsChar s1 s2 index1 index2 = True
  | index2 >= length s2 = False
  | otherwise = if equalsChar s1 s2 index1 index2 then findChars s1 s2 (index1+1) (index2+1) else findChars s1 s2 index1 (index2+1)

equalsChar :: String -> String -> Int -> Int -> Bool
equalsChar s1 s2 index1 index2 
  | index1 >= length s1 || index2 >= length s2 = False
  | otherwise = s1 !! index1 == s2 !! index2

--is_match :: String -> String -> Bool
