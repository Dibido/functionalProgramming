module Char
where
import Data.Char

equal      :: String -> String -> Bool
equal s1 s2 = map toUpper s1 == map toUpper s2

isNumeral  :: String -> Bool
isNumeral [] = error "String has length 0"
isNumeral [x] = any(True==)([n == x | n <- ['0'..'9']])
isNumeral (x:xs) = any(True==)([n == x | n <- ['0'..'9']]) && isNumeral xs

isBlank    :: String -> Bool
isBlank []     = error "String has length 0"
isBlank [x]    = x == ' '
isBlank (x:xs) = (x == ' ') && (isBlank xs)

fromDigit  :: Char -> Int
fromDigit c 
  | not (isNumeral [c]) = error "Value is not a number"
  | otherwise = ord c - ord '0' -- The letters are at 65 on the ascii table, when we subtract 48 ('0') we get the numbers on the ascii table.

toDigit    :: Int -> Char
toDigit n
  | n >= 10 || n < 0 = error "Number is not a single positive digit"
  | otherwise = chr (n + 48) --48 is the first number on the ascii table

shift      :: Int -> Char -> Char
shift n c
  | isBlank [c] = c
  | n < 0 = error "Shift number should be positive"
  | ord c < ord 'A' || ord c > ord 'Z' = error "Char is not a capital letter."
  | (ord c) + n > ord 'Z' = chr(((ord c) + n) - 26) -- Overflow past Z, return to A
  | otherwise = chr (ord c + n)

msg  ::  String
msg  =  "MHILY LZA ZBHL XBPZXBL MVYABUHL HWWPBZ JSHBKPBZ \
        \JHLJBZ KPJABT HYJUBT LZA ULBAYVU"

-- Using this function we found the original message (see decodedMessage)
shiftMessage :: String -> Int -> String
shiftMessage s n
  | s == "" = error "String is empty"
  | otherwise = map (shift n) s

decodedMessage = shiftMessage msg 19
