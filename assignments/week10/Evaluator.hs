module Evaluator
where

infixl 6 :+:
infixl 7 :*:
infixr 1 :?:

data Expr
  =  Lit Integer    -- a literal
  |  Expr :+: Expr  -- addition
  |  Expr :*: Expr  -- multiplication
  |  Div Expr Expr  -- integer division
  |  Expr :?: Expr  -- non-deterministic choice
  |  Var String     -- a variable

evalA :: (Applicative f) => Expr -> f Integer
evalA (Lit i)      =  pure i
evalA (e1 :+: e2)  =  pure (+)  <*> evalA e1 <*> evalA e2
evalA (e1 :*: e2)  =  pure (*)  <*> evalA e1 <*> evalA e2
evalA (Div e1 e2)  =  pure div  <*> evalA e1 <*> evalA e2


toss  ::  Expr
toss  =  Lit 0 :?: Lit 1

-- 11.1

evalN :: Expr -> [Integer]
evalN (Lit i)     = pure i
evalN (e1 :+: e2) = [a+b | a<-(evalN e1), b<- (evalN e2)]
evalN (e1 :*: e2) = [a*b | a<-(evalN e1), b<- (evalN e2)]
evalN (Div e1 e2) = [a `div` b | a<-(evalN e1), b<-(evalN e2), b /= 0]
evalN (e1 :?: e2) = (evalN e1) ++ (evalN e2)

tossTest1 = evalN toss
tossTest2 = evalN (toss :+: Lit 2 :*: toss)
tossTest3 = evalN (toss :+: Lit 2 :*: (toss :+: Lit 2 :*: (toss :+: Lit 2 :*: toss)))

-- 11.2.1

newtype Environ a = EN { fromEN :: [(String, Integer)] ->  a }

instance Functor Environ where
-- fmap :: (a -> b) -> Environ a -> Environ b
   fmap f (EN x) = EN $ \env -> f (x env) 

instance Applicative Environ where
-- pure :: a -> Environ a
   pure x = EN $ \env -> x
-- (<*>) :: Environ (a -> b) -> Environ a -> Environ b 
   EN f <*> EN x  = EN $ \env -> f env (x env)

instance Monad Environ where
-- return a -> Environ a
   return     = pure
-- (>>=) :: Environ a -> (a -> Environ b) -> Environ b
   EN m >>= f = error "not implemented"

evalR :: Expr -> Environ Integer
evalR (Lit i)         = EN $ \env -> i
evalR (Var a)         = EN $ \env -> result (lookup a env) where
  result (Just i) = i
  result Nothing = 0
evalR (e1 :+: e2)     = pure (+) <*> evalR e1 <*> evalR e2
evalR (e1 :*: e2)     = pure (*) <*> evalR e1 <*> evalR e2

testEvalR1 = fromEN (evalR (Var "a" :+: Lit 1)) [("a", 4711), ("b", 0815)]
testEvalR2 = fromEN (evalR (Var "a" :*: Var "b")) [("a", 4711), ("b", 0815)]
testEvalR3 = fromEN (evalR (Var "a" :*: Var "c")) [("a", 4711), ("b", 0815)]

-- 11.2.2

newtype EnvND a = EnN { fromEnN :: [(String, Integer)] ->  [a] }

instance Functor EnvND where
-- fmap :: (a -> b) -> EnvND a -> EnvND b
   fmap f (EnN x) = EnN $ \env -> map f (x env)

instance Applicative EnvND where
--pure a -> EnvND a
  pure x = EnN $ \env -> [x]
--(<*>) :: EnvND (a -> b) -> EnvND a -> EnvND b 
  EnN f <*> EnN xs  = EnN $ \env -> [f x | f <- (f env), x <- (xs env)]

evalNR :: Expr -> EnvND Integer
evalNR (Lit i)         = EnN $ \env -> [i]
evalNR (Var a)         = EnN $ \env -> result (lookup a env) where
  result (Just i) = [i]
  result Nothing = [0]
evalNR (e1 :+: e2)     = pure (+) <*> evalNR e1 <*> evalNR e2
evalNR (e1 :*: e2)     = pure (*) <*> evalNR e1 <*> evalNR e2
evalNR (e1 :?: e2)     = EnN $ \env -> (fromEnN x env) ++ (fromEnN y env) 
  where x = evalNR e1
        y = evalNR e2

testEvalNR1 = fromEnN (evalNR (Var "a" :?: Lit 1)) [("a", 4711), ("b", 0815)]
testEvalNR2 = fromEnN (evalNR (Var "a" :?: Var "b")) [("a", 4711), ("b", 0815)]
testEvalNR3 = fromEnN (evalNR (Var "a" :?: Var "c")) [("a", 4711), ("b", 0815)]
