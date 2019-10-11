module MapReduce
where
import Data.List
import Hardware hiding ((||))
import Data.Monoid hiding (All, getAll, Any, getAny)

reduce  ::  (Monoid m) => [m] -> m
reduce  =  foldr (<>) mempty 

newtype All = All { getAll :: Bool }
  deriving (Eq, Show)

instance Monoid All where
  mempty = All True
  mappend (All b1) (All b2) = All (b1 && b2)

newtype Any = Any { getAny :: Bool}
  deriving (Eq, Show)

instance Monoid Any where
  mempty = Any False
  mappend (Any b1) (Any b2) = Any (b1 || b2)

--instance Monoid Bool where

newtype OrdList elem = Ord [elem]
  deriving (Show)

instance (Ord elem) => Monoid (OrdList elem) where
  mempty = Ord []
  mappend (Ord l1) (Ord l2) = Ord (foldr (\x r -> insert x r) l1 l2)

xs = Ord [1,2,3,4]
ys = Ord [3,6,8]

sort' xs = reduce $ map Ord $ map (:[]) xs

-- foldl :: (b -> a -> b) -> b -> t a -> b
--foldm :: (a -> a -> a) -> a -> ([a] -> a)
-- foldr :: (a -> b -> b) -> b -> t a -> b

foldm :: (a -> a -> a) -> a -> ([a] -> a)
foldm f empty [] = empty
foldm f empty [x] = x
foldm f empty (x:y:[]) = f x y
foldm f empty l = f (foldm f empty left) (foldm f empty right)
  where left = take middle l
        right = drop middle l
        middle = (length l) `div` 2

foldm' :: (a -> a -> a) -> a -> ([a] -> a)
foldm' f empty [] = empty
foldm' f empty [x] = x
foldm' f empty (x:y:[]) = f x y
foldm' f empty (x:y:ys) = f (f x y) (foldm' f empty ys)

kpg :: (Bit, Bit) -> (Carry -> Carry)
kpg (O,  O  )  =  \ _c  -> O  -- kill
kpg (O,  I  )  =  \ c   -> c  -- propagate
kpg (I,  O  )  =  \ c   -> c  -- propagate
kpg (I,  I  )  =  \ _c  -> I  -- generate

data KPG  =  K | P | G
