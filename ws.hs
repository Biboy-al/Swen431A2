module Main where 
import System.Environment
import System.IO  
import Control.Monad
import Data.Char (digitToInt, isSpace,isPrint)
import Text.Parsec.Expr (Operator)
import Data.List (isInfixOf)
import Text.Read (Lexeme(String))
import Data.Functor.Reverse (Reverse)

data Operand = IntVal Int
        |BoolVal Bool
        |StringVal [Char]

class OperandOps a where
        roll:: [a] -> [a]
        rollD :: [a] -> [a]
        xor :: a -> a -> Bool
        ifelse :: a -> a -> a -> a

instance OperandOps Operand where
        roll :: [Operand] -> [Operand]
        roll s  = reverse (drop 1 revS ++ take 1 revS)
                where revS = reverse s
        rollD s = reverse (last revS : init revS)
                where revS = reverse s
        xor :: Operand -> Operand -> Bool
        xor (BoolVal o1) (BoolVal o2) = (o1 || o2) && not(o1 && o2)
        ifelse  (BoolVal b) o1 o2 = if b then o1 else o2
        

instance Show Operand where
        show (IntVal n) = show n 
        show (StringVal n) = show n
        show (BoolVal n) = parseBool n

instance Num Operand where
        IntVal op1 + IntVal op2 = IntVal (op1 + op2)
        IntVal op1 - IntVal op2 = IntVal (op1 - op2)
        IntVal op1 * IntVal op2 = IntVal (op1 * op2)

instance Eq Operand where
        IntVal op1 == IntVal op2 = op1 == op2
        IntVal op1 /= IntVal op2 = op1 /= op2

instance Ord Operand where
        compare (IntVal o1) (IntVal o2) = compare o1 o2

data StackOp = Push Operand
        | Op [Char]

data Stack = Stack [Operand]

parseBool:: Bool -> String
parseBool b 
        | b = "true"
        | not b = "false"
-- formatFileName (head args)
main :: IO ()
main = do 
        args <- getArgs
        contents <- readFile (head args)
        let newName = "output.txt"
        let tokens = tokenize contents ""
        let stack = eval tokens (Stack [])
        -- mapM_ putStrLn  (createToken contents "")
        writeFile newName (printStack (revStack stack) ++ "\n")
        

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
performOp (Op "ROT") (Stack s) = Stack (roll(take 3 s) ++ drop 3 s)
performOp (Op "ROLL") (Stack (IntVal o1:s)) = Stack (roll(take o1 s) ++ drop o1 s)
performOp (Op "ROLLD") (Stack (IntVal o1:s)) = Stack (rollD(take o1 s) ++ drop o1 s)
performOp (Op "==") (Stack (o1:o2:s)) =  Stack (BoolVal (o2 == o1) : s)
performOp (Op "!=") (Stack (o1:o2:s)) =  Stack (BoolVal (o2 /= o1) : s)
performOp (Op "<=") (Stack (o1:o2:s)) =  Stack (BoolVal (o2 <= o1) : s)
performOp (Op "<") (Stack (o1:o2:s)) =  Stack (BoolVal (o2 < o1) : s)
performOp (Op ">=") (Stack (o1:o2:s)) =  Stack (BoolVal (o2 >= o1) : s)
performOp (Op ">") (Stack (o1:o2:s)) =  Stack (BoolVal (o2 > o1) : s)
performOp (Op "^") (Stack (o1:o2:s)) =  Stack (BoolVal (xor o2 o1) : s)
performOp (Op "&") (Stack (BoolVal o1:BoolVal o2:s)) =  Stack (BoolVal (o2 && o1) : s)
performOp (Op "|") (Stack (BoolVal o1:BoolVal o2:s)) =  Stack (BoolVal (o2 || o1) : s)
performOp (Op "IFELSE") (Stack (o1:o2:o3:s)) =  Stack ( ifelse o1 o3 o2: s)


-- Utility functions for checking types
isOperator :: [Char] -> Bool
isOperator c = c `elem` ["+","-","*","**","%","/","DROP","DUP","SWAP", "ROT", "ROLL","ROLLD", "IFELSE",
        "==", "!=",">","<", ">=","<=","<=>", "&","|", "^", "IFELSE"]

isInt :: String -> Bool
isInt s = case reads s :: [(Int, String)] of
        [(n, "")] -> True
        _         -> False

conBool :: [Char] -> Bool
conBool b 
        | b == "true" = True
        | b == "false" = False


-- uses guards to create an operator
createOperand::   [Char] -> Operand
createOperand n 
        | isInt n = IntVal (read n)
        | n == "true" || n == "false" = BoolVal (conBool n)
        | otherwise = StringVal n

revStack:: Stack -> Stack
revStack (Stack s) = Stack (reverse s)

-- Prints the stack
printStack:: Stack -> String
printStack (Stack [s]) = show s
printStack (Stack (s:sx)) = show s ++ "\n" ++ printStack (Stack sx)


checkNotSpace:: Char -> Bool
checkNotSpace c = not (isSpace c)

tokenize:: [Char] -> String -> [String]
tokenize [] s 
        | null s = []
        |otherwise =[reverse s]
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
        | o == " " && o == "\n" = eval ox s
        -- If next char, accumalate the op
        | isOperator o = eval ox (performOp (Op o) s)
        | otherwise = eval ox  (performOp (Push (createOperand o) ) s)