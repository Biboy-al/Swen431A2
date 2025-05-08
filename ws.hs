
module Main where 
import System.Environment
import System.IO  
import Control.Monad
import Data.Bits
import Data.Char (digitToInt, isSpace,isPrint, isDigit)
import Data.List (isInfixOf, intersperse, transpose, isPrefixOf)
import Text.Read (Lexeme(String))
import Data.Functor.Reverse (Reverse)
import System.Posix (OpenFileFlags(creat))

data Operand = IntVal Int
        |FloatVal Float
        |BoolVal Bool
        |StringVal [Char]
        |VectorVal [Operand]
        |MatrixVal [[Operand]]
        |QuoatedVal [Char]
        |LamdaVal [Char]

class OperandOps a where
        divide:: a -> a -> a
        roll:: [a] -> [a]
        rollD :: [a] -> [a]
        ifelse :: a -> a -> a -> a
        (<=>) :: a -> a -> Int
        cross :: a -> a -> a
        trans :: a-> a

instance OperandOps Operand where

        divide (IntVal o1) (IntVal o2) = IntVal (o1 `div` o2)
        divide (FloatVal o1)  (FloatVal o2) = FloatVal (o1 / o2)
        divide (FloatVal o1) (IntVal o2) = FloatVal (o1 / fromIntegral  o2)
        divide (IntVal o1) (FloatVal o2) = FloatVal (fromIntegral  o1 / o2)

        roll s  = reverse (drop 1 revS ++ take 1 revS)
                where revS = reverse s
        rollD s = reverse(last revS : init revS)
                where revS = reverse s
        ifelse  (BoolVal b) o1 o2 = if b then o1 else o2
        o1 <=> o2 = case compare o1 o2 of
                LT -> -1
                EQ -> 0
                GT -> 1
        
        cross (VectorVal [a1, a2, a3]) (VectorVal [b1,b2,b3]) = 
                VectorVal [a2 * b3 - a3 * b2, a3 * b1 - a1 * b3, a1 * b2 - a2 * b1] 
        trans (MatrixVal m) = MatrixVal (transpose m)
        


instance Show Operand where
        show (IntVal n) = show n 
        show (FloatVal n) = show n
        show (StringVal n) = show n
        show (BoolVal n) = parseBool n
        show (VectorVal n) = "[" ++ concat (intersperse ", " (map show n)) ++ "]"
        show (MatrixVal n) = "[" ++ concat (intersperse ", " (map showRow n)) ++ "]"
                where
                        showRow row = "[" ++ concat (intersperse ", " (map show row)) ++ "]"
        show (QuoatedVal n) = n
        show (LamdaVal n) = n

instance Num Operand where
        IntVal op1 + IntVal op2 = IntVal (op1 + op2)
        FloatVal op1 + FloatVal op2 = FloatVal(op1 + op2)
        FloatVal op1 + IntVal op2 = FloatVal(op1 + fromIntegral op2)
        IntVal op1 +  FloatVal op2 = FloatVal(fromIntegral op1 +  op2)
        VectorVal op1 + VectorVal op2 = VectorVal (zipWith (+) op1 op2)
        MatrixVal op1 + MatrixVal op2 = MatrixVal(zipWith (zipWith (+)) op1 op2)
        IntVal op1 - IntVal op2 = IntVal (op1 - op2)
        FloatVal op1 - FloatVal op2 = FloatVal(op1 - op2)
        IntVal op1 -  FloatVal op2 = FloatVal(fromIntegral op1 - op2)
        FloatVal op1 - IntVal op2 = FloatVal(op1 - fromIntegral op2)
        VectorVal op1 - VectorVal op2 = VectorVal (zipWith (-) op1 op2)
        MatrixVal op1 - MatrixVal op2 = MatrixVal(zipWith (zipWith (-)) op1 op2)
        IntVal op1 * IntVal op2 = IntVal (op1 * op2)
        FloatVal op1 * FloatVal op2 = FloatVal(op1 * op2)
        IntVal op1 *  FloatVal op2 = FloatVal(fromIntegral op1 *  op2)
        VectorVal op1 * VectorVal op2 = sum (zipWith (*) op1 op2)
        MatrixVal op1 * MatrixVal op2 = MatrixVal [[sum (zipWith (*) r c) | c <- transpose op2] | r <- op1]
        MatrixVal op1 * VectorVal op2 = VectorVal [sum (zipWith (*) row op2) | row <- op1]
        VectorVal op1 * MatrixVal op2 = VectorVal [sum (zipWith (*) op1 col) | col <- op2]

        fromInteger o = IntVal(fromInteger o)

instance Eq Operand where
        IntVal op1 == IntVal op2 = op1 == op2

        StringVal op1 == StringVal op2 = op1 == op2


instance Ord Operand where
        compare (IntVal o1) (IntVal o2) = compare o1 o2

data StackOp = Push Operand
        | Op [Char]

data Stack = Stack [Operand]

instance Show Stack where
        show (Stack s) = show s

-- Utility function for printing
revStack:: Stack -> Stack
revStack (Stack s) = Stack (reverse s)

printStack:: Stack -> String
printStack (Stack [s]) = show s
printStack (Stack (s:sx)) = show s ++ "\n" ++ printStack (Stack sx)


parseBool:: Bool -> String
parseBool b 
        | b = "true"
        | not b = "false"

main :: IO ()
main = do 
        args <- getArgs
        contents <- readFile (head args)
        let newName = formatFileName (head args)
        let tokens = tokenize contents "" False False 0
        let stack = eval tokens (Stack [])
        -- mapM_ putStrLn  (createToken contents "")
        writeFile newName (printStack (revStack stack) ++ "\n")

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
performOp (Op "TRANSP") (Stack (o1:s)) = Stack (trans o1:s)
performOp (Op "EVAL") (Stack (QuoatedVal o1:s)) = eval [o1] (Stack s)
performOp (Op l) s = eval (tokenize newLambda "" False False 0) newStack
        where
                (lambda, popped, newStack) = getLambdaParams l s
                newLambda = parseLambda lambda popped

-- Utility functions for checking types
isOperator :: [Char] -> Bool  
isOperator c = c `elem` ["+","-","*","**","%","/","DROP","DUP","SWAP", "ROT", "ROLL","ROLLD", "IFELSE",
        "==", "!=",">","<", ">=","<=","<=>", "&","|", "^", "IFELSE", "<<", ">>", "<<", "!", "~", "x","TRANSP", "EVAL"]

isInt :: String -> Bool
isInt s = case reads s :: [(Int, String)] of
        [(n, "")] -> True
        _         -> False

isFloat :: String -> Bool
isFloat s = case reads s :: [(Float, String)] of
        [(n, "")] -> True
        _         -> False

isVectorInt :: String -> Bool
isVectorInt s = case reads s :: [([Int], String)] of
        [(n, "")] -> True
        _         -> False

isVectorFloat :: String -> Bool
isVectorFloat s = case reads s :: [([Float], String)] of
        [(n, "")] -> True
        _         -> False

isMatrixInt :: String -> Bool
isMatrixInt s = case reads s :: [([[Int]], String)] of
        [(n, "")] -> True
        _         -> False

isMatrixFloat :: String -> Bool
isMatrixFloat s = case reads s :: [([[Float]], String)] of
        [(n, "")] -> True
        _         -> False

conBool :: [Char] -> Bool
conBool b 
        | b == "true" = True
        | b == "false" = False
        
stripQuotes:: [Char] -> [Char]
stripQuotes s
        | length s >= 2 && head s == '"' && last s == '"' = tail (init s)
        | otherwise = s

-- Function that takes a set of operators and replaces parameters
parseLambda:: String -> Stack -> String
parseLambda lambda (Stack s) = unwords (map replace (words inner))
        where
        newStack = reverse s
        inner = init (tail lambda)
        replace ('x':ds) | all (`elem` ['0'..'9']) ds =
                let i = read ds in if i < length newStack then show (newStack !! i) else "x" ++ ds
        replace token = token

-- gets lambda epxression poping the number of arguments needed
getLambdaParams:: String -> Stack -> (String, Stack, Stack)
getLambdaParams lambda (Stack s) = (cleanLambda, Stack popped, Stack rest)
        where
        n = read (takeWhile (/= '|') (filter (`notElem` " {}") lambda)) 
        popped = take n s
        rest = drop n s
        inner = drop 1 . take (length lambda - 1) $ lambda
        cleanBody = drop 1 ( dropWhile (/='|') inner )
        cleanLambda = "{ " ++ cleanBody ++ " }"

-- uses guards to create an operator
createOperand::   [Char] -> Stack -> Operand
createOperand n s
        | isInt filteredN = IntVal (read filteredN)
        | isFloat filteredN = FloatVal (read filteredN)
        | filteredN == "true" || filteredN == "false" = BoolVal (conBool filteredN)
        | isVectorInt filteredN = VectorVal(map IntVal (read filteredN))
        | isVectorFloat filteredN = VectorVal(map FloatVal (read filteredN))
        | isMatrixInt filteredN = MatrixVal (map (map IntVal) (read filteredN))
        | isMatrixFloat filteredN = MatrixVal (map (map FloatVal) (read filteredN))
        | head filteredN == '"' = StringVal (stripQuotes filteredN)
        | otherwise = QuoatedVal filteredN
        where 
                filteredN = if head n == '\'' then drop 1 n else n

tokenize:: [Char] -> String -> Bool -> Bool -> Int -> [String]
tokenize [] s _ _ _
        | null s = []
        |otherwise =[reverse s]
tokenize (o:ox) s quoted brackted count 

        --If it's curly braces make brackted true
        | '{' == o && not quoted = tokenize ox (o : s) quoted (not brackted) count
        | '}' == o && not quoted = tokenize ox (o : s) quoted (not brackted) count
        -- if it's bracketed keep creating token
        | brackted = tokenize ox (o : s) quoted brackted count
        -- if it's a open square bracket incriment it 
        | '[' == o && not quoted = tokenize ox (o : s) quoted brackted (count + 1)
        -- ig it's a closed square bracker deincriment it
        | ']' == o && not quoted = tokenize ox (o : s) quoted brackted (count - 1)
        -- if it's going to be a string keep creating a token until not quated
        | count > 0 = tokenize ox (o : s) quoted brackted count
        | '"' == o = tokenize ox (o : s) (not quoted) brackted count
        -- If we're still in quoated just keep creating the token
        | quoted = tokenize ox (o : s) quoted brackted count
        -- if it's not a space keep creating the token
        | not (isSpace o) = tokenize ox (o : s) quoted brackted count
        | not (null s) = reverse s : tokenize ox [] quoted brackted count
        -- if it's a space skip it 
        | otherwise = tokenize ox [] quoted brackted count

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
        | head o == '{' = eval ox (performOp (Op o) s)
        | otherwise = eval ox  (performOp (Push (createOperand o s) ) s)