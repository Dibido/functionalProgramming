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
f6 = Const 5 :+: Id
f7 = Const 0 :*: Id :*: Id :+: Const 5 :*: Id
f8 = Const 0

applyPrim :: Primitive -> (Double -> Double)
applyPrim (Sin) x = sin x
applyPrim (Cos) x = cos x
applyPrim (Exp) x = exp x

apply    :: Function -> (Double -> Double)
apply (Const r) x = fromRational r
apply (Id) x = x
apply (Prim f) x = applyPrim f x
apply (f1 :+: f2) x = (apply f1 x) + (apply f2 x)
apply (f1 :*: f2) x = (apply f1 x) * (apply f2 x)
apply (f1 :.: f2) x = apply f1 (apply f2 x)

primDerivative :: Primitive -> Function
primDerivative (Sin) = Prim Cos
primDerivative (Cos) = Const (-1) :*: Prim Sin
primDerivative (Exp) = Prim Exp

derive   :: Function -> Function
derive (Const r)    = Const 0
derive (Id)         = Const 1
derive (Prim f)     = primDerivative f
derive (f1 :+: f2)  = (derive f1) :+: (derive f2)
derive (f1 :*: f2)  = (derive f1) :*: f2 :+: (derive f2) :*: f1
derive (f1 :.: f2)  = ((derive f1) :.: f2) :*: (derive f2)

simplifiedFunction :: Function -> Function
simplifiedFunction (Const 0 :+: f2) = simplifiedFunction f2
simplifiedFunction (f1 :+: Const 0) = simplifiedFunction f1
simplifiedFunction (Const 0 :*: f2) = Const 0 -- Use maybe?
simplifiedFunction (f1 :*: Const 0) = Const 0
simplifiedFunction (f1 :*: f2) = simplifiedFunction f1 :*: simplifiedFunction f2
simplifiedFunction (f1 :+: f2) = simplifiedFunction f1 :+: simplifiedFunction f2
simplifiedFunction (Const x) = Const x
