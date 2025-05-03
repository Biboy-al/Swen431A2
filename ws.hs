module Main where 
import System.Environment
import System.IO  
import Control.Monad

data StackOpe = Push Char
        | Add
        
main :: IO ()
main = do 
        args <- getArgs
        contents <- readFile (head args)
        let newName = formatFileName (head args)
        let stack = eval (process contents) []
        writeFile newName stack
        

process :: [Char] -> [Char]
process input = "START\n" ++ input ++ "\nEND"

-- Formats the name of the file to have output at the start
formatFileName :: String -> String
formatFileName s = "output-"++ drop 1 ( dropWhile (/= '-') s)

-- Performs operaton on the stack based the command
performOp:: StackOpe -> [Char] -> [Char]
performOp (Push n) s = n : s
performOp Add (o1:o2:s) = o1: o2 : s 

-- Checks if a char is an operator
isOperator :: Char -> Bool
isOperator c = c `elem` "+-*/"

-- Main Evaluation function for the stack
eval:: [Char] -> [Char] -> [Char]
eval [] s = s
eval (o:ox) s 
        | isOperator o = eval ox (performOp Add s)
        | otherwise = eval ox (performOp (Push o) s)