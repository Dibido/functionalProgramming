import Parser
import Control.Applicative

infixl 9 :@

data Lambda var
  =  Var var                   -- variable
  |  Fun var (Lambda var)      -- abstraction/λ-expression
  |  Lambda var :@ Lambda var  -- application
  deriving (Show)

testLambda0 = Var "x" 
testLambda1 = Fun 0 (Var 0)
testLambda2 = Fun "x" (Var "x")

-- 5.4
-- Grammar for Lambda var
-- Should look like the following :
-- "a \\b->b"
-- thus, variable is just 'a'
-- and function is "\\x->y"
-- and application is simply "a b", with handling the parentheses
-- So we use the \\x->x syntax and allow only single letter variable names
{-
This gives the following grammar:
  expr := term :@ expr
    | letter :@ letter
    | term

  term := '\\' letter '->' expr
    | factor

  factor := letter
    | '(' expr ')'
-}

-- In code form:
expr, term, factor :: Parser (Lambda String)
expr = do 
        x <- term; space; y <- expr; return (x :@ y)
        <|> do x <- letter; space ; y <- letter ; return ((Var [x]) :@ (Var [y]))
        <|> do term
term = do 
        space; string "\\"; space ; x <- letter; space ; string "->"; space; y <- expr; return (Fun [x] (y))
        <|> do factor
factor = do x <- letter; return (Var [x])
        <|> do char '(' ; i <- expr ; char ')'; return i

testParse1 = "a"
testParse2 = "\\x -> y"
testParse3 = "a b"
testParse4 = "a \\x->y"
testParse5 = "a b c"
testParse6 = "\\x->\\y->y"

-- Function that pretty prints the value in the Lambda var
pretty :: Lambda String -> String
pretty (Var x) = x
pretty (Fun a b) = "\\" ++ a ++ "->" ++ pretty b
pretty (a :@ b) = "(" ++ pretty a ++ " " ++ pretty b ++ ")"
