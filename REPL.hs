-- REPL.hs
-- Ramy Shahin
-- Dec. 15th 2016
import Arith

interpret :: String -> IO ()
interpret s =
    case (parseArith s) of
        Left e  -> putStr "Error: " >> (putStrLn (show e))
        Right x -> prettyPrint x

repl :: IO ()
repl = do
    l <- getLine
    if l == ":q" 
    then return ()
    else do
        interpret l
        repl

main = repl
    
