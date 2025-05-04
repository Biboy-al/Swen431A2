module Main where 
import System.Environment
import System.IO  
import Control.Monad
import Data.Char (digitToInt, isSpace,isPrint)
import Text.Parsec.Expr (Operator)
import Data.List (isInfixOf)
import Text.Read (Lexeme(String))


data Operand = IntVal Int
        | StringVal [Char]

instance Show Operand where
        show (IntVal n) = show n 
        show (StringVal n) = show n

instance Num Operand where
        IntVal op1 + IntVal op2 = IntVal (op1 + op2)
        IntVal op1 - IntVal op2 = IntVal (op1 - op2)
        IntVal op1 * IntVal op2 = IntVal (op1 * op2)

data StackOp = Push Operand
        | Op [Char]

data Stack = Stack [Operand]
        
-- formatFileName (head args)
main :: IO ()
main = do 
        args <- getArgs
        contents <- readFile (head args)
        let newName = "output.txt"
        let tokens = tokenize contents ""
        let stack = eval tokens (Stack [])
        -- mapM_ putStrLn  (createToken contents "")
        writeFile newName (printStack stack  ++ "\n")
        

process :: [Char] -> [Char]
process input = input

-- Formats the name of the file to have output at the start
formatFileName :: String -> String
formatFileName s = "output-"++ drop 1 ( dropWhile (/= '-') s)

-- Performs operaton on the stack based the command
performOp:: StackOp -> Stack -> Stack
performOp (Push n) (Stack s) = Stack (n : s)
performOp (Op "+") (Stack (o1:o2:s)) = Stack (o2 + o1 : s)
performOp (Op "-") (Stack (o1:o2:s)) = Stack (o2 - o1 : s)
performOp (Op "*") (Stack (o1:o2:s)) = Stack (o2 * o1 : s)
performOp (Op "/") (Stack (IntVal o1:IntVal o2:s)) = Stack ( IntVal (o2 `div` o1) : s)
performOp (Op "%") (Stack (IntVal o1:IntVal o2:s)) = Stack ( IntVal (o2 `mod` o1) : s)
performOp (Op "**") (Stack (IntVal o1:IntVal o2:s)) = Stack ( IntVal (o2 ^ o1) : s)
performOp (Op "DROP") (Stack (IntVal o1:s)) = Stack s
performOp (Op "DUP") (Stack (IntVal o1:s)) = Stack (IntVal o1:IntVal o1:s)
performOp (Op "SWAP") (Stack (o1:o2:s)) = Stack ( o2:o1:s)

-- Utility functions for checking types
isOperator :: [Char] -> Bool
isOperator c = c `elem` ["+","-","*","/","DROP","DUP","SWAP"]

normSpaces:: [Char] -> [Char]
normSpaces = map (\c -> if isSpace c then ' ' else c)


isInt :: String -> Bool
isInt s = case reads s :: [(Int, String)] of
        [(n, "")] -> True
        _         -> False

-- uses guards to create an operator
createOperand::   [Char] -> Operand
createOperand n 
        | isInt n = IntVal (read n)
        | otherwise = StringVal n


-- Prints the stack
printStack:: Stack -> String
printStack (Stack []) = "" 
printStack (Stack (s:sx)) = show s ++ printStack (Stack sx)


checkNotSpace:: Char -> Bool
checkNotSpace c = not (isSpace c)

tokenize:: [Char] -> String -> [String]
tokenize [] s = [reverse s]
tokenize (o:ox) s
        | not (isSpace o) = tokenize ox (o : s) 
        | not (null s) = reverse s : tokenize ox []
        | otherwise = tokenize ox []


-- Main Evaluation function for the stack
-- [Char] is the outputfile
-- Stack is an array of operands
eval:: [String] -> Stack -> Stack
eval [] s = s
eval (o:ox)  s 
        -- If current char is not a newline or space accumalte
        | o == "  " = eval ox s
        -- If next char, accumalate the op
        | isOperator o = eval ox (performOp (Op o) s)
        | otherwise = eval ox  (performOp (Push (createOperand o) ) s)