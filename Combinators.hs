module Combinators where
import Data.Char

newtype Parser a = Parser (String -> [(a, String)])

apply :: Parser a -> String -> [(a, String)]
apply (Parser p) s = p s

parse :: Parser a -> String -> a
parse p = fst . head . apply p

instance Functor Parser where
    fmap f p = Parser (\s -> map (\(x,s') -> (f x, s')) (apply p s))

instance Applicative Parser where
    pure = return
    (<*>) p q = do
        f <- p
        a <- q
        return (f a)

instance Monad Parser where
    return x = Parser (\s -> [(x, s)])
    p >>= q = Parser (\s -> [(y, s'') | (x, s') <- apply p s, (y, s'') <- apply (q x) s'])

-- failing parser
failP :: Parser a
failP = Parser (\s -> [])

-- single character parser
getc :: Parser Char 
getc = Parser f where
        f [] = []
        f (c : cs) = [(c, cs)]

-- single character matching
sat :: (Char -> Bool) -> Parser Char
sat p = getc >>= (\c -> if p c then return c else failP)

char :: Char -> Parser ()
char c = sat (==c) >> return ()

lower, upper :: Parser Char
lower = sat isLower
upper = sat isUpper

digit :: Parser Int 
digit = (sat isDigit) >>= (\d -> return (cvt d))
    where cvt d = fromEnum d - fromEnum '0'

-- string parser
string :: String -> Parser ()
string "" = return ()
string (x:xs) = char x >> string xs >> return ()

-- choice
(<|>) :: Parser a -> Parser a -> Parser a
p <|> q = Parser f
    where f s = let ps = apply p s in 
                    if null ps then apply q s else ps

-- none
none :: Parser [a]
none = return []

-- optional
optional :: Parser [a] -> Parser [a]
optional p = p <|> none

-- repitition (zero or more)
many :: Parser a -> Parser [a]
many p = optional (some p)

-- repitition (one or more)
some :: Parser a -> Parser [a]
some p = do {x <- p; xs <- many p; return (x:xs)}

-- white spaces
space :: Parser ()
space = many (sat isSpace) >> return ()

-- some string parsers
lowers :: Parser String
lowers = many lower

symbol :: String -> Parser ()
symbol xs = space >> string xs

token :: Parser a -> Parser a
token p = space >> p

