-- Fibonacci numbers
fib n = if n < 2 then 1 else fib(n-1) + fib(n-2)

-- Factorial
fact n = if n == 0 then 1 else n * fact(n-1)

-- Absolute value
abs x = if x >= 0 then x else -x

-- Max value
max x y = if x > y then x else y

-- Absolute value with guards and explicit types
absg :: Int -> Int
absg x  | x >= 0 = x
        | x < 0 = -x