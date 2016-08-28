{-
  Calulator parser

  number ::= [ "-" ] digit { digit }.
  digit  ::= "0" | "1" | ... | "8" | "9".
  expr   ::= term { addop term }.
  term   ::= factor { mulop factor }.
  factor ::= "(" expr ")" | number.
  addop  ::= "+" | "-".
  mulop  ::= "*".

-}
module Test.Calculator

import NanoParsec.Parser
import NanoParsec.Combinators


data Expr = Add Expr Expr
          | Mul Expr Expr
          | Sub Expr Expr
          | Lit Int
          deriving Show

eval :: Expr -> Int
eval ex = case ex of
    Add a b -> eval a + eval b
    Mul a b -> eval a * eval b
    Sub a b -> eval a - eval b
    Lit n   -> n

int :: Parser Expr
int = number >>= \n -> return (Lit n)

expr :: Parser Expr
expr = term `chainl1` addop

term :: Parser Expr
term = factor `chainl1` mulop

factor :: Parser Expr
factor = int <|> parens expr

infixOp :: String -> (a -> a -> a) -> Parser (a -> a -> a)
infixOp x f = reserved x >>= \_ => return f

addop :: Parser (Expr -> Expr -> Expr)
addop = (infixOp "+" Add) <|> (infixOp "-" Sub)

mulop :: Parser (Expr -> Expr -> Expr)
mulop = infixOp "*" Mul
