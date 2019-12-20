{-# LANGUAGE TypeFamilies #-}

module Minimax2
where
import Squiggol

data Tree elem  =  Node elem [Tree elem]

--13.2.1

instance Functor (TREE elem) where
  fmap f (NODE e st) = NODE e $ map f st

data TREE elem tree = NODE elem [tree]
  
instance Base (TREE elem) where
  type Rec (TREE elem) = Tree elem
  inn (NODE e ns) = Node e ns
  out (Node e ns) = NODE e ns

--13.2.2
size, depth :: Tree elem -> Integer
size = fold $ \(NODE e es) -> 1 + sum es
depth = fold $ addLayer
  where addLayer (NODE e []) = 1
        addLayer (NODE e es) = 1 + maximum es

--13.2.3
gametree :: (position -> [position]) -> (position -> Tree position)
gametree f = unfold (\pos -> NODE pos (f pos))

--13.2.4
winning  :: Tree position -> Bool
winning = fold (\(NODE _ subTree) -> any not subTree)
