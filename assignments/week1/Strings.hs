module Strings 
where

head :: String -> Char
head s = s !! 0

tail :: String -> String
tail (x:xs) = xs

is_equal :: String -> String -> Bool
is_equal s1 s2 = s1 == s2

is_substring :: String -> String -> Bool
is_substring s1 s2 = subString s1 s2

subString :: String -> String -> Bool
subString (x1:xs1) (x2:xs2) = x1 == x2 && subString xs1 xs2

-- Example is_substring "there" "is someone out there" => True

-- is_sub ::

-- is_match ::
