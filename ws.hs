module Main where 
import System.Environment
import System.IO  
import Control.Monad

data Operator = Data

main :: IO ()
main = do 
        args <- getArgs
        contents <- readFile (head args)
        let newName = formatFileName (head args)
        writeFile newName (process contents)     

process :: [Char] -> [Char]
process input = "START\n" ++ input ++ "\nEND"

formatFileName :: String -> String
formatFileName s = "output-"++ drop 1 ( dropWhile (/= '-') s)

eval :: [Char] -> Operator
eval [] = ()
eval (s:sx) = 
