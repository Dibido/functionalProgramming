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
--See Char.hs
--2.3
{-
If we compute large factorials using :: Int we see that for prod [1..n] with n >= 65 gives a result of 0. We think this is the case because Int has only 64 bits available. When computing large factorials, the numbers always end 
--with a set of 0 digits. For example, 65! (Using Integer) is something like 544....416000000000000. This tail of 0's does never shrink, it only grows. The binary representation of integer will therefore also end with a tail of 0's.
--Int takes 64 bits from the right of the total number, these will then also be only 0-bits. Thats why it shows a result of 0. The reason we observe :: Int showing a negative number sometimes, is because the bits overflow and the 
most significant bit (which indicates positive/negative) gets touched with (too) large numbers.
-}
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
--2.5.1
-- well formed = returns a value (http://learnyouahaskell.com/functionally-solving-problems)
-- well typed = doesnt get stuck (https://en.wikipedia.org/wiki/Type_safety)
-- (+4), not well formed or typed
-- div, not well formed or typed
-- div 7, not well formed or typed
-- (div 7) 4, well formed and well typed
-- div (7 4), not well formed or typed
-- 7 `div` 4, well formed and typed
-- + 3 7, not well formed or typed
-- (+) 3 7, well formed and typed
-- (b, 'b', "b"), well formed and typed
-- (abs, 'abs', "abs"), not well formed or typed (because of 'abs')
-- abs . negate, not well formed but well typed, can be assigned to a variable.
-- (* 3) . (+ 3), not well formed but well typed, can be assigned to a variable.
--2.5.2
-- (abs .) . (. negate),
-- (div .) . (. mod), 
--2.5.3
-- i :: a -> a
-- k :: (a,b) -> a
-- b :: ((c->b->a),b,c) -> a
-- c :: (a,b,c) ->
-- s :: (a,b,c) ->
--2.6
--2.7
-- f1 :: a -> a
-- f2 :: a -> Int
-- f3 :: (a, b) -> (b, a)
-- f4 :: (a, b) -> (b, a)
-- f5 :: a -> b -> (a,b)
-- f6 :: a -> b -> (a,(b,b),a)
-- f7 :: (a,(b,c)) = ((a,b),c)
--2.8
-- f1 :: [a] -> a -> Int -> Int
-- f2 :: [a] -> a -> Int
-- f3 :: String -> Int -> String
-- f4 :: String -> String
-- f5 :: [a] -> Int -> Int -> [a]
-- f6 :: [a] -> [a]
