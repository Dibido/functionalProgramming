-- Assignments Week 5 Dibran Dokter s1047390 & Marnix Lukasse s1047400
--5.1
allTrue :: [Bool] -> Bool
allTrue l = foldl (&&) True l
allFalse :: [Bool] -> Bool
allFalse l = foldl (\x r -> not x && r) True l
member :: (Eq a) => a -> [a] -> Bool
member m l = foldr (\x r -> if m == x then True else r) False l
smallest :: [Int] -> Int
smallest l = foldr (\x r -> if x<r then x else r) (maxBound::Int) l
largest :: [Int] -> Int
largest l = foldr (\x r -> if x>r then x else r) (-maxBound::Int) l
--5.2
-- See Numeral.hs
