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

-- Filter cretes a new list by testing each element with a Boolean

-- True if the list is not empty and False if the list is empty
notNull xs = not (null xs)
isEven x = x `mod` 2 == 0
removeOdd = filter isEven


-- FOLD
-- is used to combine all elements in the list into a single value
-- two versions of fold : foldl  and foldr
-- show function is Haskell to String to convert the number x
showPlus s x = "(" ++ s ++ "+" ++ show x ++ ")"
showPlus' x s = "(" ++ (show x) ++ "+" ++ s ++ ")"

-- The choice between foldl vs. foldr is easy. Use that one that actually works.
-- Foldl: slightly faster, cannot be used on infite list
-- Foldr: can be used on infite list

-- Zip

listPairs = zip [1,2,3] [4,5,6]

-- zipWith
-- if you want to combine elements other than Pairs. Use zipWith
listSum = zipWith (+) [1,2,3] [3,4,5]
plus3 x y z = x + y + z


-- Function Operators
-- (.) - Function composition operator. It takes two functions and combines them into a new function
-- ($) - Function application operator. Which takes a function and a value and applies the function to the value
-- The order in function composition is important. The composed functions are applied right to left
stringLength = length . show
stringLenght' x = length (show x)
notNull' = not . null

-- Applicaiton Operator $
-- We can use the application operator to avoid parenthesis. 
-- f $ x = f x
-- f $ g x = f (g x)
-- f $ g $ h $ k x = f (g (h (k x)))

fA = map (\f -> f 3) [(+1), (\x -> 2 * x + 3), (*2)]
fA' = map ($3) [(+1), (\x -> 2 * x + 3), (*2)]
fAZ = zipWith ($) [(+1), (\x -> 2 * x + 3), (*2)] [1,2,3]

-- Summary
-- Functions are values 
-- Can be created on the fly and passed around
-- Higher-order-functions: take other functions, map, filter, fold, zipWith



main :: IO()
main =  do
  putStrLn (show (map length ["hello", "abc", "12345"]))
  putStrLn (show (map (1+) [1,2,3,4,5]))
  putStrLn (show (double [1,2,3,4,5]))
  -- true if the element should be kept and false if the element should be removed
  putStrLn (show (filter notNull ["" ,"abc", "", "hello", ""]))
  putStrLn (show (removeOdd [1,2,3,4,5,6]))
  putStrLn (show (map snd (filter fst [(True, 1), (False, 7), (True, 11)])))
  putStrLn (show (foldl (+) 0 [1,2,3,4]))
  -- foldl starts on the left
  putStrLn (foldl showPlus "0" [1,2,3,4]) -- ((((0+1)+2)+3)+4)
  -- foldr starts on the right
  putStrLn (foldr showPlus' "0" [1,2,3,4]) -- (1+(2+(3+(4+0))))
  putStrLn (show listPairs)
  putStrLn (show listSum) -- [4, 6, 8]
  putStrLn (show (zipWith3 plus3 [1,2,3] [1,2,3] [1,2,3])) -- [3, 6, 9]
  -- Lambda expressions
  putStrLn (show (zipWith3 (\x y z ->  x + y + z) [1,2,3] [1,2,3] [1,2,3]))
  putStrLn (show (map (\x -> 2 * x) [1,2,3]))
  -- show is the toString function in Haskell. It converts everything to a string. Since putStrLn expectes a string we need explicitly to convert the values
  putStrLn (show (stringLength "Hello World!")) -- 14
  putStrLn (show (notNull' [1,2,3]))
  putStrLn ((show . notNull') [1,2,3])
