module BinaryTree
where

import Data.List

data Tree elem = Empty | Node (Tree elem) elem (Tree elem)
  deriving (Show)

instance Functor Tree where
  fmap _f Empty         =  Empty
  fmap f  (Node l a r)  =  Node (fmap f l) (f a) (fmap f r)

ex1  ::  Tree Integer
ex1  =  Node Empty 4711 (Node Empty 0815 (Node Empty 42 Empty))
ex2  ::  Tree String
ex2  =  Node (Node (Node Empty "Frits" Empty) "Peter" Empty) "Ralf" Empty
ex3  ::  Tree Char
ex3  =  Node (Node Empty 'a' Empty) 'k' (Node Empty 'z' Empty)


binTree :: Tree Char
binTree = Node (Node Empty 'a' (Node Empty 'b' Empty)) 'c' (Node (Node Empty 'd' Empty) 'f' (Node Empty 'g' Empty))

size :: Tree elem -> Int 
size (Empty) = 0
size (Node leftElem elem rightElem) = 1 + size leftElem + size rightElem

minHeight :: Tree elem -> Int
minHeight (Empty) = 0
minHeight (Node leftElem elem rightElem) = 1 + min (minHeight leftElem) (minHeight rightElem)

maxHeight :: Tree elem -> Int
maxHeight (Empty) = 0
maxHeight (Node leftElem elem rightElem) = 1 + max (maxHeight leftElem) (maxHeight rightElem)

member :: (Eq elem) => elem -> Tree elem -> Bool
member n (Empty) = False
member n (Node leftElem elem rightElem) = n == elem || member n leftElem || member n rightElem

preorder :: Tree elem -> [elem]
preorder (Empty) = []
preorder (Node leftElem elem rightElem) = [elem] ++ preorder leftElem ++ preorder rightElem

inorder :: (Ord elem) => Tree elem -> [elem]
inorder (Empty) = []
inorder (Node leftElem elem rightElem) = sort (preorder (Node leftElem elem rightElem))

postorder :: Tree elem -> [elem]
postorder (Empty) = []
postorder (Node leftElem elem rightElem) = postorder leftElem ++ postorder rightElem ++ [elem]

layout :: (Show elem) => Tree elem -> String
layout (Empty) = "" 
layout (Node leftElem elem Empty) = "***" ++ layout leftElem ++ show elem ++ "\n"
layout (Node Empty elem rightElem) = show elem ++ "\n" ++ "___" ++ layout rightElem
layout (Node leftElem elem rightElem) = ("---" ++ layout leftElem) ++ (show elem ++ "\n") ++ ("+++" ++ layout rightElem)

build :: [elem] -> Tree elem
build [] = Empty
build l = build (first (splitAt listMiddle l)) : l : build (last (splitAt listMiddle l)) where listMiddle = (length l / 2)

-- balanced :: [elem] -> Tree elem
-- create :: Int -> Tree ()
