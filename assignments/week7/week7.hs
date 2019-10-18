-- Assignments Week 7 Dibran Dokter s1047390 & Marnix Lukasse s1047400
--7.4
-- foldbtree combines all the tips using a function(monoid) to combine all the tips into a single value of the same type.
-- tips simply makes a list containing all the values in the tips for a given tree.

-- map f (tips t) = tips (mapBTree f t)
-- IH = map f (tips t) = tips (mapBTree f t)
--                                                 Step used to convert
-- base case : 
-- assume t = Tip a
-- map f (tips (Tip a))                           - (Assumption)
-- map f (foldBTree (++) (mapBTree (:[]) Tip a)   - (5)
-- map f (foldBTree (++) (Tip ((:[]) a)           - (1)
-- map f (foldBTree (++) (Tip [a])                - (Using :[])
-- map f ([a])                                    - (3)
-- f a                                            - ([a] has only one element because ((:[]) a)
-- f a                                            - (3 Reverse)
-- foldBTree (++) (Tip [(f a)]                    - (Using :[] Reverse)
-- foldBTree (++) (Tip ((:[]) Tip (f a)           - (1 Reverse)
-- foldBTree (++) (mapBTree ((:[]) (Tip (f a))    - (5 Reverse)
-- tips (Tip (f a))                               - (1 Reverse)
-- tips (mapBTree f (Tip a))                      - (Assumption)

-- We prove the base case using the above steps.

-- induction step :
-- assume t = Bin (BTree a) (BTree a)
-- map f (tips (Bin (BTree a) (BTree a)))         - (Assumption)
-- map f (foldBTree (++) (mapBTree (:[]) (Bin (BTree a) (BTree a))  - (5)
-- map f (foldBTree (++) (Bin (MapBTree (:[]) BTree a) (MapBTree (:[]) BTree a)    - (2)
-- Here we assume that Btree a = Tip a. If this would not be the case, the last 2 lines would keep going recursively until it eventually becomes Tip a.
-- map f (foldBTree (++) (Bin (MapBTree (:[]) Tip a) (MapBTree (:[]) Tip a)
-- map f (foldBTree (++) (Bin (Tip ((:[]) a) (Tip ((:[]) a))                       - (1)
-- map f (foldBTree (++) (Bin (Tip [a]) (Tip [a]))                                 - (Using :[])
-- map f ((++) (foldBTree (++) (Tip [a]) (foldBTree (++) (Tip [a])                 - (4)
-- map f ((++) (([a]) ([a]))                                                       - (3)
-- map f ([a] ++ [a])                                                              - (By definition)
-- (map (++) [a]) ++ (map (++) [a])                                                - (Using 7.3.1)


-- foldBTree (++) (mapBTree (:[]) (Bin (Tip (f a)) (Tip (f a))))                  - (5)
-- tips (Bin (Tip (f a)) (Tip (f a)))                                             - (1)
-- tips (Bin (mapBTree f (Tip a)) (MapBtree f (Tip a)))                           - (Assumption)
-- tips (Bin (mapBTree f (BTree a)) (mapBTree f (BTree a)))                       - (2)
-- tips (mapBTree f (Bin (BTree a) (BTree a))     - (Assumption)
