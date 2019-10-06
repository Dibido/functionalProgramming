module Hardware
where
import Data.List
import Prelude hiding ((||))

data Bit  =  O | I
  deriving (Eq, Ord, Show)

-- For carry, when 1 ^^ 1 then 1
infixr 3 ∧∧
(∧∧) :: Bit -> Bit -> Bit
O ∧∧ _b  =  O
I ∧∧ b   =  b

-- OR
infixr 2 ||
(||) :: Bit -> Bit -> Bit
O || b   =  b
I || _b  =  I

-- For sum, when 1 >< 1 then 0
infixr 4 ><
(><) :: Bit -> Bit -> Bit
O >< O  =  O
O >< I  =  I
I >< O  =  I
I >< I  =  O

mapr :: ((a, state) -> (b, state)) -> ([a], state) -> ([b], state)

--mapr f (as, state) = foldr (\x r -> (fst (f (x, snd(r)) : r, snd (f (x, snd(r)))))) ([],0) as

mapr fA (xs, carry) = (map fst $ fullResult, snd (head fullResult))
  where fullResult = reverse $ unfoldr (\(x, y) -> if (null x) then Nothing else Just (fA (head x, y), ((tail x), snd $ fA (head x, y)))) (reverse xs, carry)

-- (mapr :: (((Bit, Bit), Carry) -> (Bit, Carry) -> ([(Bit, Bit)], Carry) -> ([Bit], Carry)) fullAdder $ (zip [I, I, O, O] [I, O, I, O], O) 

type Carry  =  Bit

halfAdder :: (Bit, Bit) -> (Bit, Carry)
halfAdder (a, b) = (a >< b, a ∧∧ b)

fullAdder :: ((Bit, Bit), Carry) -> (Bit, Carry)
fullAdder ((a, b), cin) = ((a >< b) >< cin, ((a><b) ∧∧ cin) || a ∧∧ b)
