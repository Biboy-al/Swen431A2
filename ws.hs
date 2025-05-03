module Main where 
import System.Environment
import System.IO  
import Control.Monad

data StackOpe = Push Char
        
main :: IO ()
main = do 
        args <- getArgs
        contents <- readFile (head args)
        let newName = formatFileName (head args)
        let stack = eval (process contents) []
        writeFile newName stack
        

process :: [Char] -> [Char]
process input = "START\n" ++ input ++ "\nEND"

formatFileName :: String -> String
formatFileName s = "output-"++ drop 1 ( dropWhile (/= '-') s)

performOp:: StackOpe -> [Char] -> [Char]
performOp (Push n) s = n : s

eval:: [Char] -> [Char] -> [Char]

eval [] s = s
eval (o:ox) s = eval ox (performOp (Push o) s)