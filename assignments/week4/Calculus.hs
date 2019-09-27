module Calculus
where

data Primitive
  =  Sin  -- trigonometric: sine
  |  Cos                          -- cosine
  |  Exp  -- exponential
  deriving (Show)

infixl 6 :+:
infixl 7 :*:
infixr 9 :.:

data Function
  =  Const Rational         -- constant function
  |  Id                     -- identity
  |  Prim Primitive         -- primitive function
  |  Function :+: Function  -- addition of functions
  |  Function :*: Function  -- multiplication of functions
  |  Function :.: Function  -- composition of functions
  deriving (Show)

f1 = Const 5
f2 = Id
f3 = Const 2 :*: Id :*: Id :+: Const 5 :*: Id
f4 = Const 2 :*: Prim Sin :+: Id
f5 = Id :*: Prim Cos :.: (Id :*: Id)

applyPrim :: Primitive -> (Double -> Double)
apply (

apply    :: Function -> (Double -> Double)
apply (Const r) x = r
apply (Id) x = x
apply (Prim f) x = f x
--apply (f1 :+: f2) x = 
--apply (f1 :*: f2) x = 
--apply (f1 :.: f2) x = 


-- derive   :: Function -> Function
-- simplify :: Function -> Function
