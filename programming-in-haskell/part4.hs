module Part4 where

{-
  - Pattern matching. In general pattern matching is syntactic sugar for the if clause
  - <> is the append operator. ++ works only on lists and string. <> works on almost everything.
-}

-- in case of greet "Dimitri" it's goint to return "Hey Haskell learner, how are you doing today?"
greet "Dimitri" = "Hey Haskell learner, how are you doing today?"
-- in case of greet "Claus" it's going to return "Hello Claus"
greet name = "Hello " <> name

-- Repetition of the previous parts
-- In general in Haskell, a function is a mapping that takes one or more arguments and produces a single result.
-- Here the functiont takes a type String as an argument und produces IO() as a single result. It prints to the screen!
sayHello :: String -> IO()
sayHello x = putStrLn("Hello, " <> x <> "!")

-- In Haskell the sum can be defined using two equations
-- The first equation states that the sum of empty list is zero.
-- The second equation states that the sum of non-empty list comprising a first number `n`and a remaining list of numbers `ns`is given by adding `n` and the sum of `ns`
-- Note the sum function is recursive, it reduces the list by 1 until the list is empty [] and the recursion stops. 
mySum [] = 0
mySum (n:ns) = n + mySum(ns)

-- where keyword introduces local defintions
-- qsort [x] has no effect on singletons, it returns the single element of the list
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
               where
                smaller = [a | a <- xs, a <= x]
                larger = [b | b <- xs, b > x]

-- if the list is empty we return an empty list of results, otherwise we perform the first action in the list, then perform the remaining actions in the lit. Ad finally return the list of results that we produced
-- One possible type: seqn :: [IO a] -> IO [a] that means seqn maps a list of IO action to produce results of some type a to a single IO that produces a list of such results.
seqn [] = return []
seqn (act:acts) = do x <- act
                     xs <- seqn acts
                     return (x:xs)

-- In Haskell all computations are done via the evaluation of expressions to yield values (answers)
-- Every value has a type. A type is a set of values.
-- Atomic values are: Int 5, Char 'a', Bool -> Bool A Function, [x] A list, (Char, Int) A pair/tuple.
-- :: can be read has a type: 5 :: Int, 'a' :: Char, not :: Bool -> Bool, [1, 2, 3] :: [Int], ('a', 2) :: (Char, Int)
-- A function is defined as a series of equations
inc :: Int -> Int
inc n = n + 1

-- GHCi is the interactive interpreter
-- Prelude is the built-in library with a range of functions that operate e.g. on list

-- Any function with two arguments can be written between its arguments by enclosing the name of the function in single back ``.

-- This type say that myDiv takes first arg, second arg and return `a` type that is of the same type of the arguments. `a` in this case in `Integral`
-- type `:type div` to see the type of expression. Good to learn how the types of functions are constructed.
myDiv :: Integral a => a -> a -> a
myDiv x y = x `div` y

-- A type is a collection of related values. Bool contains the two logical values False and True. While Bool -> Bool contains all functions that map arguments from Bool to result of Bool.
myNot :: Bool -> Bool
myNot b = b

-- In Haskell every expression must have a type, which is calculated prior to evaluating the expression by a process called **type inference**
-- The rule: if `f`is a function that maps arguments of type `a` to results of type `b` and `e` is an expression of type `a`, then application of `f e` has a type `b`

-- The number of components in a tuple is called its arity. The tuple () of arity zero is called the empty tuple, tuples of arity two are called paris, tuples of arity three are called triples and so on. 
-- The tuples of arity one, such as (False) are not premitted because they would conflict with the use of parentheses to make the evaluation order explicit, such as in (1 + 2) * 3. 

tupleId :: (Int, Int) -> (Int, b)
tupleId (a, b) = (a, b)

-- Function types
-- A function is a mapping from arguments of one type to results of another type. We write T1 -> T2 for the type of all functions that map arguments of type T1 to result of T2.
{-
myNotB :: Bool -> Bool
myEven :: Int -> Bool
-}


