======================================================================================================
================================ NOT MY WORK! Sample code from course ================================
======================================================================================================

Read and evaluate a simple arithmetic expression

An expression is made up of 1 digit numbers, operators (+. -, *, /).
"/" is treated as integer division.

Quote from email:
"If you want to look at the program more closely and try to understand it, please feel free to do so.
 In this case, you might like to work out what the error in the program is - please email me if you 
 think you have found it.  You might also like to extend the program to allow multi-digit numbers, 
 to allow spaces between symbols (numbers and operators), to allow ^ as an exponentiation operator,
 or to allow brackets.  Doing exercise like this is the best way to learn and new language!"

> import Data.Char

> type IExp = String

> data Symbol = Op Char | Val Int
>               deriving (Eq, Show)

> type RExp = [Symbol]

> type OpStack = [Char]

> type ValStack = [Int]

Read an expression as a string, convert to RPN, and evaluate it.

> readEval :: String -> Int
> readEval = eval . inFixToRPN

Read an expression as a string, convert to RPN

> inFixToRPN :: IExp -> RExp
> inFixToRPN iexp = inFixToRPN' iexp [] []

Actions are:
 - stop: when input and stack are both empty*
 - shift from input to output: when next input is number
 - pop from stack to output: when input is empty*, or next symbol has lower
   precedence than that at top of stack 
 - push from input to stack: when stack is empty*, or next symbol has higher
   precedence than that at top of stack

Auxiliary function, using a stack to convert to RPN.

> inFixToRPN' :: IExp -> OpStack -> RExp -> RExp
> inFixToRPN' [] [] rexp = rexp                                     -- stop
> inFixToRPN' [] (stackOp:stack) rexp =
>           inFixToRPN' [] stack (rexp ++ [Op stackOp])             -- pop1
> inFixToRPN' (sym:syms) stack rexp 
>   | isDigit sym =
>       inFixToRPN' syms stack (rexp ++ [Val (digitToInt sym)])     -- shift
> inFixToRPN' (sym:syms) [] rexp = inFixToRPN' syms [sym] rexp      -- push1
> inFixToRPN' (sym:syms) (stackOp:stack) rexp
>   | (prec sym) < (prec stackOp)
>       = inFixToRPN' (sym:syms) stack (rexp ++ [Op stackOp])       -- pop2 
>   | otherwise = inFixToRPN' syms (sym:stackOp:stack) rexp         -- push2

Define operator precedence

> prec op
>   | op == '+' || op == '-' = 1
>   | op == '*' || op == '/' = 2

Evaluate an expression in RPN form

> eval :: RExp -> Int
> eval exp = eval' exp []

Auxiliary function to evaluate and RPN expression using a stack.

> eval' :: RExp -> ValStack -> Int
> eval' [] [x] = x                                    -- stop
> eval' (Val x : exp') stack = eval' exp' (x : stack) -- push
> eval' (Op op : exp) (x : y : stack') =
>       eval' exp (apply op x y : stack')             -- applyop

Apply an operator to two numbers

> apply :: Char -> Int -> Int -> Int
> apply '+' x y = x + y
> apply '-' x y = x - y
> apply '*' x y = x * y
> apply '/' x y = x `div` y