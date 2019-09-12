--2.1.1
-- Bool only has two values to only two functions with type Bool -> Bool exist
-- Identity : True -> True and False -> False
identity :: Bool -> Bool
identity x = x
-- Negation : True -> False and False -> True
negation :: Bool -> Bool
negation x = not x
--2.1.2
-- (Bool, Bool) -> Bool allows all boolean operators like &&,||, xor etc. Also the set operators fst and snd
fst' :: (Bool, Bool) -> Bool
fst' (x,_) = x
snd' (_,x) = x
and' (x,y) = x && y
or' (x,y) = x || y
xor' (x,y)
  | and' (x, y) = False
  | otherwise = or' (x, y)
--2.1.3
-- Bool -> Bool -> Bool is similar to the previous case in that it takes 2 bools and returns one, except it is not a set
-- So it has the same boolean operators as above
--2.2
--See Char.lhs
--2.3
-- The problem arises because the Int datatype is limited to 32 bits and 66! is greater than 2^32 so the Int datatype wraps back to 0
-- Before the number gets so high that it wraps the 32 bits it flips the most significant bit which is used to denote negative numbers, so it goes to a huge negative number first
-- With the Integer datatype this problem does not occur because the datatype is infinite
--2.4
--2.4.1
swap :: (Int,Int) -> (Int, Int)
swap (x,y) = (y,x)
double :: (Int, Int) -> (Int, Int)
double (x,y) = ((x*2), (y*2))
square :: (Int, Int) -> (Int, Int)
square (x,y) = ((x*x), (y*y))
--2.4.2
-- The swap function is still valid because you can swap any type.
-- Then the functions double and square will fail, because those are operations on Ints.
--2.4.3
-- The first is a set of two values with a set with two values as the second value
-- type : (a, (b,c))
-- The second is a triple set with 3 different types
-- type : (a, b, c)
convert :: (a, (b,c)) -> (a,b,c)
convert (a, (b,c)) = (a,b,c)
--2.5
--2.8
-- f1 :: [a] -> a -> Int -> Int
-- f2 :: [a] -> a -> Int
-- f3 :: String -> Int -> String
-- f4 :: String -> String
-- f5 :: [a] -> Int -> Int -> [a]
-- f6 :: [a] -> [a]
