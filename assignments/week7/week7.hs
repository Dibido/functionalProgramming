-- Assignments Week 7 Dibran Dokter s1047390 & Marnix Lukasse s1047400
--7.4
-- foldbtree combines all the tips using a function(monoid) to combine all the tips into a single value of the same type.
-- tips simply makes a list containing all the values in the tips for a given tree.

-- map f (tips t) = tips (mapBTree f t)
-- IH = map f (tips t) = tips (mapBTree f t)
--                                                                                Step used to convert
-- base case : 
-- assume t = Tip a
-- map f (tips (Tip a))                                                            - (Assumption)
-- map f (foldBTree (++) (mapBTree (:[]) Tip a)                                    - (5)
-- map f (foldBTree (++) (Tip ((:[]) a)                                            - (1)
-- map f (foldBTree (++) (Tip [a])                                                 - (Using :[])
-- map f ([a])                                                                     - (3)
-- f a                                                                             - ([a] has only one element because ((:[]) a)
-- f a                                                                             - (3 Reverse)
-- foldBTree (++) (Tip [(f a)]                                                     - (Using :[] Reverse)
-- foldBTree (++) (Tip ((:[]) Tip (f a)                                            - (1 Reverse)
-- foldBTree (++) (mapBTree ((:[]) (Tip (f a))                                     - (5 Reverse)
-- tips (Tip (f a))                                                                - (1 Reverse)
-- tips (mapBTree f (Tip a))                                                       - (Assumption)

-- We prove the base case using the above steps.

-- induction step :
-- assume t = Bin (BTree a) (BTree a)
-- map f (tips (Bin (BTree a) (BTree a)))                                          - (Assumption)
-- map f (foldBTree (++) (mapBTree (:[]) (Bin (BTree a) (BTree a))                 - (5)
-- map f (foldBTree (++) (Bin (MapBTree (:[]) BTree a) (MapBTree (:[]) BTree a)    - (2)
-- Here we assume that Btree a = Tip a. If this would not be the case, the last 2 lines would keep going recursively until it eventually becomes Tip a.
-- map f (foldBTree (++) (Bin (MapBTree (:[]) Tip a) (MapBTree (:[]) Tip a)
-- map f (foldBTree (++) (Bin (Tip ((:[]) a) (Tip ((:[]) a))                       - (1)
-- map f (foldBTree (++) (Bin (Tip [a]) (Tip [a]))                                 - (Using :[])
-- map f ((++) (foldBTree (++) (Tip [a]) (foldBTree (++) (Tip [a])                 - (4)
-- map f ((++) (([a]) ([a]))                                                       - (3)
-- map f ([a] ++ [a])                                                              - (By definition)
-- (map f [a]) ++ (map f [a])                                                      - (Using 7.3.1)
-- ([f a]) ++ ([f a])                                                              - (By definition)
-- (++) ([f a]) ([f a])                                                            - (3 Reverse)
-- (++) (foldBTree (++) (Tip [f a])) (foldBTree (++) (Tip [f a]))                  - (4 Reverse)
-- foldBTree (++) (Bin (Tip [f a]) (Tip [f a])                                     - (Using :[])
-- foldBTree (++) (Bin (Tip (:[]) (f a)) (Tip (:[]) (f a))                         - (1 Reverse)
-- foldBTree (++) (Bin (mapBTree (:[]) Tip (f a)) (mapBTree (:[]) Tip (f a)))      - (2 Reverse)
-- foldBTree (++) (mapBTree (:[]) (Bin (Tip (f a)) (Tip (f a))))                   - (5 Reverse)
-- tips (Bin (Tip (f a)) (Tip (f a)))                                              - (1 Reverse)
-- tips (Bin (mapBTree f (Tip a)) (MapBtree f (Tip a)))                            - (Assumption)
-- tips (Bin (mapBTree f (BTree a)) (mapBTree f (BTree a)))                        - (2 Reverse)
-- tips (mapBTree f (Bin (BTree a) (BTree a))                                      - (Assumption)

-- Using the above steps we prove the induction step.

-- 7.6.1
-- foldr fusion law:
-- f (g a b) = h a (f b) -> f (foldr g e xs) = foldr h (f e) xs

-- foldr g e . map f = foldr (g . f) e

-- Define the map as a foldr by doing to following:
-- foldr (\x r -> f x : r) [] xs

-- Write down the equality as the RHS
-- foldr g e . (foldr (\x r -> f x : r) [] xs) = foldr (g . f) e xs
-- foldr g (e (foldr (\x r -> f x : r) [] xs)) = foldr g (f e) xs
-- Identify f, g, h
-- f = foldr g e .
-- g = \x r -> f x : r
-- h = g
-- Prove the LHS
-- foldr g e . (b : a) ?= g a (foldr g e . b)
-- At this point we got confused and stuck, we didn't know how to proceed. One of the confusing things was that map f takes no arguments. So there is no list that f can be mapped on.

-- 7.6.2
-- foldr g e . map f = foldr (g . f) e

-- map (f . g) = map f . map g

-- write map as a foldr
-- foldr (\x r -> f x : r) [] xs

-- Write down equality as the RHS of the foldr-map fusion law
-- 

-- Identify f, g, h
-- Prove the LHS


-- We tried for a while to make any sense of this, but really had no clue how to proceed with exercise 7.6. Not sure whether its because we hardly have any knowledge of proofs at all (new at uni) or were just too dumb.
