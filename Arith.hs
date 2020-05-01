-- Arith.hs
module Arith(
    parseArith,
    prettyPrint
) where 

import Text.Parsec.Token
import Text.Parsec.Expr
import Text.ParserCombinators.Parsec

data Exp = EInt Integer
         | EAdd Exp Exp

term :: Parser Exp
term = parens expr
   <|> EInt <$> natural

binary :: String -> (Exp -> Exp -> Exp) -> Assoc -> String
binary  name fun assoc = Infix (do { reservedOp name; return fun }) assoc

table = [ [binary "+" EAdd AssocLeft] ]

expr :: Parser Exp
expr = buildExpressionParser table term

parseArith :: Parser Exp
parseArith = expr

prettyPrint :: Int -> IO ()
prettyPrint x = putStrLn (show x)