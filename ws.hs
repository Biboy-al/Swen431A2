module Main where 
import System.Environment
import System.IO  
import Control.Monad
import Data.Char (digitToInt)
import Text.Parsec.Expr (Operator)

data Operand = IntVal Int

instance Show Operand where
        show (IntVal n) = show n 

instance Num Operand where
        IntVal op1 + IntVal op2 = IntVal (op1 + op2)
        IntVal op1 - IntVal op2 = IntVal (op1 - op2)
        IntVal op1 * IntVal op2 = IntVal (op1 * op2)


data StackOp = Push Operand
        | Op Char

data Stack = Stack [Operand]
        deriving (Show)
        

main :: IO ()
main = do 
        args <- getArgs
        contents <- readFile (head args)
        let newName = formatFileName (head args)
        let stack = eval (process contents) (Stack [])
        writeFile newName (printStack stack)
        

process :: [Char] -> [Char]
process input = input

-- Formats the name of the file to have output at the start
formatFileName :: String -> String
formatFileName s = "output-"++ drop 1 ( dropWhile (/= '-') s)

-- Performs operaton on the stack based the command
performOp:: StackOp -> Stack -> Stack
performOp (Push n) (Stack s) = Stack (n : s)
performOp (Op '+') (Stack (o1:o2:s)) = Stack (o2 + o1 : s)
performOp (Op '-') (Stack (o1:o2:s)) = Stack (o2 - o1 : s)
performOp (Op '*') (Stack (o1:o2:s)) = Stack (o2 * o1 : s)
performOp (Op '/') (Stack (IntVal o1:IntVal o2:s)) = Stack ( IntVal (o2 `div` o1) : s)
performOp (Op '%') (Stack (IntVal o1:IntVal o2:s)) = Stack ( IntVal (o2 `mod` o1) : s)

-- Utility functions for checking types
isOperator :: Char -> Bool
isOperator c = c `elem` "+-*/"



isInt :: String -> Bool
isInt s = case reads s :: [(Int, String)] of
        [(n, "")] -> True
        _         -> False

-- uses guards to create an operator
createOperand::   [Char] -> Operand
createOperand n 
        | isInt n = IntVal (read n)
        | otherwise = IntVal 0


-- Prints the stack
printStack:: Stack -> String
printStack (Stack []) = "" 
printStack (Stack (s:sx)) = show s ++ "\n" ++ printStack (Stack sx)

-- Main Evaluation function for the stack
-- [Char] is the outputfile
-- Stack is an array of operands
eval:: [Char] -> Stack -> Stack
eval [] s = s
eval (o:ox) s 
        | isOperator o = eval ox (performOp (Op o) s)
        | o == '\n' || o == ' ' = eval ox s
        | otherwise = eval ox (performOp (Push (createOperand [o]) ) s)