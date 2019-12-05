import Data.Maybe

infixl 9 :@:

data Expr
  =  Lit Integer      -- a literal
  |  Var String       -- a variable
  |  Bin Expr Op Expr -- binary operator
  |  Abs String Expr  -- a lambda expression
  |  Expr :@: Expr    -- an application
  deriving Show

data Op = Plus | Mult
  deriving Show

data Value = IntVal Integer | FunVal Env String Expr
  deriving Show
  
type Env = [(String,Value)]

applyIntOp :: Op -> Value -> Value -> Value
applyIntOp op (IntVal v1) (IntVal v2) = 
   case op of 
      Plus -> IntVal (v1 + v2)
      Mult -> IntVal (v1 * v2)
applyIntOp op x y = errorMsg "op failed"

eval :: Expr -> Env -> Value
eval (Lit i)        env = IntVal i
eval (Var v)        env = fromJust (lookup v env)
eval (Bin e1 op e2) env = applyIntOp op (eval0 e1 env) (eval0 e2 env)
eval (Abs v b)      env = FunVal env v b
eval (ef :@: ea)    env = let FunVal env var body = eval0 ef env
                              arg                 = eval0 ea env
                           in eval0 body ((var,arg):env) 
                           
myExpr = Bin (Lit 12) Plus ((Abs "x" (Bin (Var "x") Mult (Lit 2))) :@: Bin (Lit 4) Plus (Lit 2))

-- 11.4.1
-- Eval in monadic style
{-# LANGUAGE.DeriveFunctor #-}

newtype EvalM a = EvalM { unEvalM :: Env -> Either String a}

instance Applicative EvalM where
  pure = return
  (<*>) = ap 

instance Monad EvalM where
  return x = EvalM $ \_env -> Right x
  ex >>= ey = EvalM $ \env -> do 
    x <- unEvalM ex env
    unEvalM (ey x) env

errorMsg :: String -> EvalM a
errorMsg err = EvalM $ \_ -> Left err

getEnv :: EvalM Env
getEnv = EvalM $ \env -> Right env 

withEnv :: Env -> EvalM a -> EvalM m
withEnv newEnv ea = EvalM $ \_oldEnv -> unEvalM ea newEnv

eval0 :: Expr -> Env -> Value
eval0 (Lit i)        = return $ IntVal i
eval0 (Var v)        = do
  env <- getEnv
  case lookup v env of
    Nothing -> errorMsg $ "error" ++ v
    Just x -> return x
eval0 (Bin e1 op e2) =  do 
  x1 <- eval e1
  x2 <- eval e2  
  applyIntOp op x1 x2
eval0 (Abs v b)      = do
  env <- getEnv
  return $ FunVal env v b
eval0 (ef :@: ea)    = do 
   f <- eval ef
   case f of
    FunVal env var body -> do
      arg <- eval0 ea
      withEnv ((var,arg):env) $ do 
        eval body
    _ -> errorMsg $ "error" ++ show f

eval1 :: Expr -> Env -> Value
eval1 (Lit i) env        = IntVal i
eval1 (Var v) env        = fromJust (lookup v env)
eval1 (Bin e1 op e2) env = do
  let i1 = eval1 e1 env
  let i2 = eval1 e2 env
  applyIntOp op i1 i2
eval1 (Abs v b) env      = FunVal env v b
eval1 (ef :@: ea) env    = do
  let fun = eval1 ef env
  let arg = eval1 ea env
  case fun of FunVal env' var body -> eval1 body ((var, arg):env)

-- 11.4.2

eval2 :: Expr -> Env -> Either String Value
eval2 (Lit i) env        = Right (IntVal i)
eval2 (Var v) env        = case lookup v env of 
  Nothing -> Left "Error"
  Just x -> Right x 
eval2 (Bin e1 op e2) env = do
  let i1 = eval1 e1 env
  let i2 = eval1 e2 env
  applyIntOp op i1 i2
eval2 (Abs v b) env      = FunVal env v b
eval2 (ef :@: ea) env    = do
  let fun = eval1 ef env
  let arg = eval1 ea env
  case fun of FunVal env' var body -> eval1 body ((var, arg):env)


{- 
After this we were unable to find a solution, mainly because we had problems understanding the assignment itself.

-}
