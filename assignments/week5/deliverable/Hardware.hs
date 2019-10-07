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

mapr fA (xs, carry) = (map fst $ fullResult, snd (head fullResult))
  where fullResult = reverse $ unfoldr (\(x, y) -> if (null x) then Nothing else Just (fA (head x, y), ((tail x), snd $ fA (head x, y)))) (reverse xs, carry)

test1 = mapr fullAdder $ (zip [O,O,O,I] [O,O,O,I], O)
test2 = mapr fullAdder $ (zip [O,O,O,I] [O,O,O,I], I)
test3 = mapr fullAdder $ (zip [I,I,I,I] [I,I,I,I], I)
test4 = mapr fullAdder $ (zip [O,I,I,I] [I,I,I,I], I)
test5 = mapr fullAdder $ (zip [O,I,O,I] [I,O,I,O], O)
test6 = mapr fullAdder $ (zip [I,O,O,O] [I,O,O,O], O)

type Carry  =  Bit

halfAdder :: (Bit, Bit) -> (Bit, Carry)
halfAdder (a, b) = (a >< b, a ∧∧ b)

fullAdder :: ((Bit, Bit), Carry) -> (Bit, Carry)
fullAdder ((a, b), cin) = ((a >< b) >< cin, ((a><b) ∧∧ cin) || a ∧∧ b)
