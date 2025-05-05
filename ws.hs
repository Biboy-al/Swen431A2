
module Main where 
import System.Environment
import System.IO  
import Control.Monad
import Data.Bits
import Data.Char (digitToInt, isSpace,isPrint)
import Text.Parsec.Expr (Operator)
import Data.List (isInfixOf, intersperse)
import Text.Read (Lexeme(String))
import Data.Functor.Reverse (Reverse)
import System.Posix (OpenFileFlags(creat))

data Operand = IntVal Int
        |FloatVal Float
        |BoolVal Bool
        |StringVal [Char]
        |VectorVal [Int]
        |MatrixVal [Operand]

class OperandOps a where
        divide:: a -> a -> a
        -- (**):: a -> a -> a
        roll:: [a] -> [a]
        rollD :: [a] -> [a]
        -- xr :: a -> a -> Bool
        ifelse :: a -> a -> a -> a
        (<=>) :: a -> a -> Int
        cross :: a -> a -> a

instance OperandOps Operand where
        divide :: Operand -> Operand -> Operand
        divide (IntVal o1) (IntVal o2) = IntVal (o1 `div` o2)
        divide (FloatVal o1)  (FloatVal o2) = FloatVal (o1 / o2)
        divide (FloatVal o1) (IntVal o2) = FloatVal (o1 / fromIntegral  o2)
        divide (IntVal o1) (FloatVal o2) = FloatVal (fromIntegral  o1 / o2)

        -- (IntVal o1) ** (IntVal o2)  = IntVal (o1 ^ o2)
        -- (FloatVal o1) ** (IntVal o2)  = FloatVal (o1 ^ o2)
        -- (FloatVal o1) ** (FloatVal o2)  = FloatVal (o1 ^ o2)
        roll :: [Operand] -> [Operand]
        roll s  = reverse (drop 1 revS ++ take 1 revS)
                where revS = reverse s
        rollD s = reverse(last revS : init revS)
                where revS = reverse s
        -- xr (BoolVal o1) (BoolVal o2) = (o1 || o2) && not(o1 && o2)
        ifelse  (BoolVal b) o1 o2 = if b then o1 else o2
        o1 <=> o2 = case compare o1 o2 of
                LT -> -1
                EQ -> 0
                GT -> 1
        
        cross (VectorVal [a1, a2, a3]) (VectorVal [b1,b2,b3]) = VectorVal [a2 * b3 - a3 * b2, a3 * b1 - a1 * b3, a1 * b2 - a2 * b1] 
        

instance Show Operand where
        show (IntVal n) = show n 
        show (FloatVal n) = show n
        show (StringVal n) = show n
        show (BoolVal n) = parseBool n
        show (VectorVal n) = "[" ++ concat (intersperse ", " (map show n)) ++ "]"
        show (MatrixVal n) = "[" ++ concat (intersperse ", " (map show n)) ++ "]"

instance Num Operand where
        IntVal op1 + IntVal op2 = IntVal (op1 + op2)
        FloatVal op1 + FloatVal op2 = FloatVal(op1 + op2)
        FloatVal op1 + IntVal op2 = FloatVal(op1 + fromIntegral op2)
        IntVal op1 +  FloatVal op2 = FloatVal(fromIntegral op1 +  op2)
        VectorVal op1 + VectorVal op2 = VectorVal(zipWith (+) op1 op2)
        IntVal op1 - IntVal op2 = IntVal (op1 - op2)
        FloatVal op1 - FloatVal op2 = FloatVal(op1 - op2)
        IntVal op1 -  FloatVal op2 = FloatVal(fromIntegral op1 -  op2)
        IntVal op1 * IntVal op2 = IntVal (op1 * op2)
        FloatVal op1 * FloatVal op2 = FloatVal(op1 * op2)
        IntVal op1 *  FloatVal op2 = FloatVal(fromIntegral op1 *  op2)
        VectorVal op1 * VectorVal op2 = IntVal(sum (zipWith (*) op1 op2))
        

        --fromInteger (IntVal op1) = fromInteger op1

instance Eq Operand where
        IntVal op1 == IntVal op2 = op1 == op2

        StringVal op1 == StringVal op2 = op1 == op2


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
        let tokens = tokenize contents "" False 0
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
performOp (Op "+") (Stack (StringVal o1:StringVal o2:s)) = Stack (StringVal (o2 ++ o1): s)
performOp (Op "+") (Stack (o1:o2:s)) = Stack (o2 + o1 : s)
performOp (Op "-") (Stack (o1:o2:s)) = Stack (o2 - o1 : s)
performOp (Op "*") (Stack (IntVal o1:StringVal o2:s)) = Stack (StringVal (concat (replicate o1 o2)): s)
performOp (Op "*") (Stack (o1:o2:s)) = Stack (o2 * o1 : s)
performOp (Op "/") (Stack (o1:o2:s)) = Stack ( divide o2 o1 : s)
performOp (Op "%") (Stack (IntVal o1:IntVal o2:s)) = Stack ( IntVal (o2 `mod` o1) : s)
performOp (Op "**") (Stack (IntVal o1:IntVal o2:s)) = Stack ( IntVal (o2 ^ o1) : s)
performOp (Op "DROP") (Stack (o1:s)) = Stack s
performOp (Op "DROP") (Stack (StringVal o1:s)) = Stack s
performOp (Op "DUP") (Stack (o1:s)) = Stack (o1:o1:s)
performOp (Op "DUP") (Stack (StringVal o1:s)) = Stack (StringVal o1:StringVal o1:s)
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
performOp (Op "<=>") (Stack (o1:o2:s)) =  Stack (IntVal (o2 <=> o1) : s)
performOp (Op "^") (Stack (BoolVal o1: BoolVal o2:s)) =  Stack (BoolVal ((o1 || o2) && not(o1 && o2)) : s)
performOp (Op "&") (Stack (BoolVal o1:BoolVal o2:s)) =  Stack (BoolVal (o2 && o1) : s)
performOp (Op "|") (Stack (BoolVal o1:BoolVal o2:s)) =  Stack (BoolVal (o2 || o1) : s)
performOp (Op "IFELSE") (Stack (o1:o2:o3:s)) =  Stack ( ifelse o1 o3 o2: s)
performOp (Op "<<") (Stack (IntVal o1:IntVal o2:s)) =  Stack (IntVal (shiftL o2 o1) : s)
performOp (Op ">>") (Stack (IntVal o1:IntVal o2:s)) =  Stack (IntVal (shiftR o2 o1) : s)
performOp (Op "^") (Stack (IntVal o1:IntVal o2:s)) =  Stack (IntVal (xor o2 o1) : s)
performOp (Op "!") (Stack (BoolVal o1:s)) =  Stack (BoolVal (not o1) : s)
performOp (Op "~") (Stack (IntVal o1:s)) =  Stack (IntVal (complement o1) : s)
performOp (Op "x") (Stack (o1:o2:s)) =  Stack ( cross o2 o1 : s)
-- Utility functions for checking types
isOperator :: [Char] -> Bool
isOperator c = c `elem` ["+","-","*","**","%","/","DROP","DUP","SWAP", "ROT", "ROLL","ROLLD", "IFELSE",
        "==", "!=",">","<", ">=","<=","<=>", "&","|", "^", "IFELSE", "<<", ">>", "<<", "!", "~", "x"]

isInt :: String -> Bool
isInt s = case reads s :: [(Int, String)] of
        [(n, "")] -> True
        _         -> False

isFloat :: String -> Bool
isFloat s = case reads s :: [(Float, String)] of
        [(n, "")] -> True
        _         -> False

isVector :: String -> Bool
isVector s = case reads s :: [([Int], String)] of
        [(n, "")] -> True
        _         -> False

isMatrix :: String -> Bool
isMatrix s = case reads s :: [([[Int]], String)] of
        [(n, "")] -> True
        _         -> False


conBool :: [Char] -> Bool
conBool b 
        | b == "true" = True
        | b == "false" = False

stripQuotes :: [Char]-> [Char]
stripQuotes s
  | length s >= 2 && head s == '"' && last s == '"' = tail (init s)
  | otherwise = s

-- uses guards to create an operator
createOperand::   [Char] -> Operand
createOperand n 
        | isInt n = IntVal (read n)
        | isFloat n = FloatVal (read n)
        | n == "true" || n == "false" = BoolVal (conBool n)
        | isVector n = VectorVal(read n)
        | isMatrix n = MatrixVal (map VectorVal (read n))
        | otherwise = StringVal (stripQuotes n)

revStack:: Stack -> Stack
revStack (Stack s) = Stack (reverse s)

-- Prints the stack
printStack:: Stack -> String
printStack (Stack [s]) = show s
printStack (Stack (s:sx)) = show s ++ "\n" ++ printStack (Stack sx)


checkNotSpace:: Char -> Bool
checkNotSpace c = not (isSpace c)

tokenize:: [Char] -> String -> Bool -> Int -> [String]
tokenize [] s _ _
        | null s = []
        |otherwise =[reverse s]
tokenize (o:ox) s quoted count
        -- if it's a open square bracket incriment it 
        | '[' == o && not quoted = tokenize ox (o : s) quoted (count + 1)
        -- ig it's a closed square bracker deincriment it
        | ']' == o && not quoted = tokenize ox (o : s) quoted (count - 1)
        -- if it's going to be a string keep creating a token until not quated
        | count > 0 = tokenize ox (o : s) quoted count
        | '"' == o = tokenize ox (o : s) (not quoted) count
        -- If we're still in quoated just keep creating the token
        | quoted = tokenize ox (o : s) quoted count
        -- if it's not a space keep creating the token
        | not (isSpace o) = tokenize ox (o : s) quoted count
        | not (null s) = reverse s : tokenize ox [] quoted count
        -- if it's a space skip it 
        | otherwise = tokenize ox [] quoted count

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
