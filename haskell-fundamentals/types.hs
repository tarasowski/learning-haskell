module Types where

import Prelude

-- Types Systems
-- Static Types                                                                         vs. Dynamic Types
-- Variables have a fixed type and can hold only values related to that type            Variables can store any type (var x = "hello")
-- int x = "hello" will not compile                                                     There is no type checking and no compiler
-- More errors caught at compile-time int x = "hello" + 3                               More run-time errors (var x = "hello" + 3")
-- Write lots of types (bad)                                                            Never write types (good)
-- Code reuse becomes harder (you need to define statically the type)                   Better code reuse
-- Repeated code for different types                                                    Same code works for many types


-- Haskell's Type System
-- It's very very statically type
--  It can catch lots of compile-time errors
--  Few run-time errors (once it has compiled)
-- Type are inferred
--  The compiler figures it out what type must be
--  Don't hae to write out explicit types
--  Explicit types communicate with PEOPLE (in Haskell), checked by compiler
--  You use types to communicate with other PEOPLE if you use them at all
-- Same code can work for many different types
--  Using polymorphic functions you can use a single function that can handle many different types


-- Explicit Types
-- When defining a variable or a function you can give it explicit type

-- TYPE DECLARATION
-- The way you communicate with people and machines
str :: [Char]
str = "hello"

foo :: Int -> Int
foo x = 2 * x + 1

add3Int :: Int -> Int -> Int -> Int
add3Int x y z = x + y + z

-- TYPE ANNOTATION
-- You never need to use type annotations in code
-- You declare an expression that is not stored in a variable
x = 3 :: Int


-- TYPE INFERENCE
-- You don't see a type declaration here. But the complier does a lot of work.
-- It seems that Haskell is like dynamically typed language, but it's not. 
square x = x * x
-- square "hello" will give us an error, because compiler knows it works only on Int
-- squareTwice x = square (square x) -- that works fine
-- brokenShowSquare x = "The sqare is: " ++ square x -- would not work, because ++ works only on String

-- When to use Explicit Types
-- Communicate with people. The type declaration ist not for the compiler. 
-- mystery :: [Char] -> Int - if you see this type declaration you know what's going on. Since Haskell is pure, there can't be some effects going on.
-- The compiler doesn't know the type from read "123" adding a type annotation fixes this
x' = show (read "123" :: Int)
-- Explicit type annotation is good for performance. But optimize if it's not needed.
bar :: Int
bar = x * y * z
  where x = 32
        y = 42
        z = -5


-- Polymorphic Function
-- One of the drawbacks of statically typed languages was the repeat of the functions (code reuse).
-- Polymorphic means that a function can work on any type of e.g. length
chars :: [Char]
chars = length "hello world"

ints :: [Int]
ints = length [1,2,3,4,5]

listOf = [[Char]]
listOf = length ["hello", "world"]

-- Type Variables
-- whenever you call the length function the compiler figures out what type a needs to be
-- ANY FUNCTION WITH A TYPE VARIABLE IS CALLED A POLYMORPHIC FUNCTION
-- type variables always start with lower case letters: a,b,x,foo,hello_123 - also called Abstract Types
-- Concrete types - start with Upper Case
-- length :: [a] -> Int

-- empty_list is a list of any type.
-- whenever empty_list is used the compiler figures out what a type has to be
empty_list :: [a]
empty_list = []

list_double :: [Double]
list_double = 3.2 : empty_list

list_char :: [Char]
list_char = 'a' : empty_list

-- the first [a] is a list of any type, and head returs the value of the same type as the element in the list
-- Repeated type variables always represent the same type
head' :: [a] -> a
head (x : xs) = x

-- different type variables can represent different types
-- e.g. it takes a list of [[Char]] and returns Int
-- badHead :: [a] -> b - the compiler will reject this function definition
-- badHead (x : xs) = x

-- Type Class Constraints
-- If you don't know the type of a function. Just ask ghci what type your function actually has
-- The double arrows indicates a CONSTRAINT on a type variable
-- In this case it says: that `a` must be a `Num` type standing for numeric
-- The `Num` type that `a` is standing for has to support a `+` operator and a way to interper a literal 0
-- `Num` types also support other things like multiplicatin and subtraction
-- sum is a function which for any numeric type a takes a list of values of type a and returns a value of type a
-- `Num` is called a type class, which means it represents all types which can do numeric things
sum :: Num a => [a] -> a
sum [] = 0
sum' (x : xs) = x + sum xs

-- show is Haskell's toString. Which takes a value and returns a string representation.
-- but you can't turn any value into a string e.g. functions don't have a meaningful representation. You can't pass a function as argument to show
-- The `Show` type class constraint says that `a` must be a type that has a String representation.
-- Function types are not in the `Show` type class
show' :: Show a => a -> [Char]

-- Multiple Type Constraints
-- A function can have multiple type class constraints.
-- Just put them into parens and separate them with commas.
-- This function requires that `a` be a an `Num` type class so that `sum` can be applied to the list of values of type `a` 
-- It also requires that a is in the `Show` type class, so the result of the `that has a type `a` has to have a sensible String representation
showSum :: (Num a, Show a) => [a] -> [Char]
showSum xs = show (sum xs)

main =
  putStrLn (show "Hello World!")
