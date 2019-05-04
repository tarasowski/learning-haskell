module Two where

-- The Healthy Eating Pyramid
-- All food is put into groups that have a common property. So we can say the food is typed!
-- The food is grouped into categories:
--  Carbs: Bread, rice and pasta are CARBS
--  Fats: Avocado, Oil, Olives, Butter
--  Protein: Meat, milk, fish, cheese, nuts

-- Types
-- Taking values (not food) and group them into categories that have similar properties
-- A type is name for a collection of related values.  A type `Bool` is a name for a collection that has two values `False | True`
-- data Bool = True | False

-- Type Errors
-- Applying a function to one or more arguments of the wrong type is called a type error.
-- > 1 + False
-- Error: 1 is a number an Flase is a logical value, but (+) required two numbers

-- The Goal of Static Typing
-- Is that the compiler will catch the errors before we run the program/script. At compile time!
-- But there are other dynamically typed languages where the errors are caught at runtime. 
-- There are non-typed languages, they will take the values and try to add them up
-- If evaluating an expression `e` would produce a value of type `t`, then `e`has type `t`, written e :: t
-- expression :: type
-- add :: Int
-- add = 1 + 2
-- Every well formed expression has a type, which can be automatically calculated at compile time using a process called type inference.
-- All type errors are found at compile time, which makes programs safer and faster by removing the need for type checks at run time.
-- Basic type (base types): Bool, Char, String, Int (fixed-precision integers), Integer (arbitrary-precision integers), Float

-- List Type
-- Is a polymorphic type or generic type
-- [t] is the type of lists with elements of type `t`. Where `t` is a type variable and can be anything (almost) anything.

-- Tuple Type
-- (t1,t2,...,tn) is the type of n-tuples who ith components have type ti for any i in 1..n.


-- Functions
-- A function is a mapping from values of one type to values of another type:
-- not        :: Bool -> Bool
-- isDigit    :: Char -> Bool -- it takes a value in the collection of characters and will return a value from the collection of Integers
-- In general: t1 -> t2 is the type of functions that maps values of type t1 to values to type t2. 
-- t1 is the domain of the function
-- t2 is the range of the function

-- Curried Functions + Partial Application
-- add     :: (Int, Int) -> Int
-- add (x,y) = x + y -- most of the languages are optimize to take tuples as arguments
-- Functions with multiple arguments are also possible by returning functions as results:
-- add'     :: Int -> (Int -> Int)
-- add' x y = x + y -- Haskell is optimized to take arguments one by one (currying)
-- Functions that take arguments one by one are called curried functions!
-- mult       :: Int -> (Int -> (Int -> Int))
-- mult x y z = x*y*z

-- Haskell is optimized for currying in constrast to other languages that take tuples of arguments.


-- Right association
-- mult     ::  Int -> Int -> Int -> Int
-- Means    ::  Int -> (Int -> (Int -> Int))

-- Left association
-- In function application the association is to the left
-- mult x y z
-- Means ((mult x) x) z

main = putStrLn "Hello World!"
