module HOF where

import Prelude
-- In Java you use function to package and reuse your code
-- In Haskell functions are much more fundamental
-- Functions in Haskell are like any other type of value like: Int, String
-- Higher order functions can take and return other functions


-- Funcitons as Values
-- Passed as function arguments
-- Crated on the fly
-- Functions can be returned from a function
pass3 f = f 3
add1 x = x + 1
mult2 x = 2 * x

compose :: (t1 -> t2) -> (t3 -> t1) -> t3 -> t2
compose f g x = f (g x)

-- There are functions that build other function at runtime on the fly
-- this is a function that is defined during compile time.
always7 x = 7

-- this is a function that is built during runtime (const build a function on the fly, it can be anything)
always7' = const 7


-- Partial Application
-- With Partial Application you can create function during runtime
-- In many languages you need to supply all arguments. But in Haskell you have partial application.

-- Operators in Haskell are all functions
-- (+), (*), (:), (++) are all functions
-- To use them as functions use parens around the operator

-- (+) 5 3
-- (*) 2 2
-- (:) [1,2] [3,4]
-- (++) "Hello" "World!"

-- we can pass the operators to other functions
pass_3_4 f = f 3 4

-- definition of new operator .+, we are using dot because + is already defined
-- we are using pattern matching here, where a is the first element and b is the second of the pair
-- for the c,d goes the same c is the first and d is the second
-- (a,b) .+ (c,d) = (a + c, b + d)

-- we can partially apply also the operators
plus1 = (+) 1

-- special syntax to partially apply an operator by supplying either the left or right argument
-- in this version the left side is 1 and the right side is the remaining function parameter
plus1' = (1+)
-- in this version the right side is 1 and the left side is the remaining function parameter
plus1'' = (+1)

-- Turning functions into operators
-- mod 10 2
-- 10 `mod` 2 --- this is so called infix operator
-- double here is a named function for map (2*)
double = map (2*)

main :: IO()
main =  do
  putStrLn (show (map length ["hello", "abc", "12345"]))
  putStrLn (show (map (1+) [1,2,3,4,5]))
  putStrLn (show (double [1,2,3,4,5]))
