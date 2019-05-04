module P1 where

-- In order to learn something you need to do a lot of exercises (sometimes pointless s. KarateKid example)
-- GHC is the leading imlementation of Haskell, and comprises a compiler and interactive repl

-- The standard prelude
-- Prelude comes with a large number of standard library functions. Prelude provides many functions on lists.
one = head [1,2,3,4,5]
five = tail [1,2,3,4,5]
three = [1,2,3,4,5] !! 2 -- in Haskell lists are not array. Indexing into second element it's going to throw away the 0th, 1st elements and return 3
first3 = take 3 [1,2,3,4,5] -- generalisation of head / tail
last2 = drop 3 [1,2,3,4,5] -- generalisation of head / tail
mult = product [1,2,3,4,5]
app = [1,2,3] ++ [4,5]
rv = reverse [1,2,3,4,5]
-- In Haskell we use often map, filter, fold and not indexing to perform complex operations over a list. You can see indexing bind to a variable `three` above.

-- Function application
-- In math, function application is denoted using parens, and multiplication os often denoted using juxtaposition or space
-- a function is applied to `a` and `b` and add `c` * `d`
-- f(a,b) + c d
-- f a b + c * d -- this is how it's written in Haskell. Function application is denoted by whitespace!!!
-- Moreover, function application is assumed to have higher priority than all other operatiors.
-- f a + b -- f applied to `a` + b

-- In Haskell programs are not called programs, typically people refer to them as scripts.
-- New functions are defined within a script, a text file comprising a sequence of definitions.
-- Haskell scripts usually have a `.hs` suffix on the filename.

factorial n = product [1..n]
average ns = sum ns `div` length ns
--                  div is enclosed in back quotes
--                  (x `f` y) is just syntactic sugar for f x y
--                  `` turns a functions into a function operator

-- Naming Requirements
-- Function and argument  names must bgin with a lower-case letter: myFun fun1 arg_2 x'
-- A type has to start with the uppercase name
-- By convention, list arguments usually have an `s` suffix on their name. For example:
-- xs (list of values of type x) ns (list of values of type n) nss (list of list because of `ss`)

-- Haskell has a common with Python that whitespaces are significant.

a = b + c
    where
      b = 1
      c = 2
d = a * 2

-- :load name
-- :reload
-- :edit name
-- :edit
-- :type expr
-- :?
-- :q quit GHCI

-- How to read errors in Haskell
-- not "Hello"
-- ERROR - Type error in application
-- Expression : not "Hello"
-- Term : "Hello"
-- Type : String
-- Does not match : Bool

main = putStrLn "Hello World"
