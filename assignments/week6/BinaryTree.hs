module BinaryTree
where

data Tree elem = Empty | Node (Tree elem) elem (Tree elem)
  deriving (Show)

instance Functor Tree where
  fmap _f Empty         =  Empty
  fmap f  (Node l a r)  =  Node (fmap f l) (f a) (fmap f r)

ex1  ::  Tree Integer
ex1  =  Node Empty 4711 (Node Empty 0815 (Node Empty 42 Empty))
ex1'  =  Node Empty 411 (Node Empty 085 (Node Empty 4 Empty))
ex2  ::  Tree String
ex2  =  Node (Node (Node Empty "Frits" Empty) "Peter" Empty) "Ralf" Empty
ex3  ::  Tree Char
ex3  =  Node (Node Empty 'a' Empty) 'k' (Node Empty 'z' Empty)

instance (Eq elem) => Eq (Tree elem) where
  Empty == Empty = True
  (Node l1 e1 r1) == (Node l2 e2 r2) = l1 == l2 && e1 == e2 && r1 == r2
  _ == _ = False

instance (Ord elem) => Ord (Tree elem) where
  Empty <= _ = True
  _ <= Empty = False
  (Node l1 e1 r1) <= (Node l2 e2 r2) = l1 <= l2 && e1 <= e2 && r1 <= r2
  
-- size :: Tree elem -> Int
-- minHeight, maxHeight :: Tree elem -> Int
-- member :: (Eq elem) => elem -> Tree elem -> Bool
-- preorder, inorder, postorder :: Tree elem -> [elem]
-- layout :: (Show elem) => Tree elem -> String
-- build :: [elem] -> Tree elem
-- balanced :: [elem] -> Tree elem
-- create :: Int -> Tree ()
