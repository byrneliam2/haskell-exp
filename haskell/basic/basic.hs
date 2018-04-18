-- Fibonacci numbers
fib n = if n < 2 then 1 else fib(n-1) + fib(n-2)

-- Factorial
fact n = if n == 0 then 1 else n * fact(n-1)

-- Absolute value
myabs x = if x >= 0 then x else -x

-- Max value
mymax x y = if x > y then x else y

-- Absolute value with guards and explicit types
absg :: Int -> Int
absg x  | x >= 0 = x
        | x < 0 = -x

-- Factorial with guards and explicit types
factg :: Int -> Int
factg x | x == 0 = 1
        | otherwise = x * fact(x-1)

-- Factorial pattern matching
factp 0 = 1
factp x = x * factp(x-1)