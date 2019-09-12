f :: (Int, Int) -> Int
f (a,_) = a

f1 :: (a,a) -> a
f1 (a,_) = a

f2 :: (a,b) -> a
f2 (a,_) = a

f3 :: Int -> (Int -> Int)
f3 x y = x + y

f4 :: (Int -> Int) -> Int
f4 x = 3 x 5
