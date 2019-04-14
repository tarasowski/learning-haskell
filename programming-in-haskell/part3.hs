module Part3 where

-- Basic concepts

{-
 - A type is a collection of related values. For example type Bool contains the two logical values False and True, while the type Bool -> Bool contains all functions that map argumens from Bool to results from Bool.
 - We use the notation `v :: T`to mean that v is a value in the type T, and say that `v` has type T. 

False :: Bool
True :: Bool
not :: Bool -> Bool

 - The symbol `::`can also be used with expressions that have not yet been evaluated, in which case the notation `e :: T` means that evaluation of the expression `e` will produce a value of type `T`. 
not False :: Bool
not True :: Bool
not (not False) :: Bool

 - In Haskell every expression must have a type, which is calculated prior to evaluating the expression by a process called **type inference.**
 - The key to this process is the following simple typing rule for function application, which states that if 
          ****######  `f` is a function that maps arguments of type `A`to results of type `B`, ######*****
            and `e`is an expression of type A, then applicaiton of `f e`has type `B`  
 - For example, the typing not False :: Bool can be inferred from this rulse using the fact that not :: Bool -> Bool and False :: Bool. 
 - On the other hand, the expression not 3 does not have a type under the above rule, because that would require 3 :: Bool, which is not valid because 3 is not a logical value. Expressions such as not 3 that do not have a type are said to contain a type rror, and are deemed to be invalid expressions.

 - ** Because of type inference precedes evaluation, Haskell programs are type safe, in the sense that type errors can never occur during evaluation.
-}  

-- Basic types
{-  
  -- Bool - logical values: this type contains the two logical values False and True
  -- Char - single characters: This type contains all single characters in the Unicode system. Single characters must be enclosed in single forward quotes ''. 
  -- String - strings of characters: This type contains all sequences of characters, such as "abc", "1+2=3", and the empty string "". Strings must be enclosed in double quotes "".
  -- Int - fixed-precision integers: This type contains integers such as -100, 0 and 999.
  -- Integer - arbitrary-precision integers: This type contains all integers, with as much memory as necessary being used for their storage. The choice between Int and Integer is also one of performance. Most computers have built-in hardware for fixed-precision integers.
  -- Float - single-precision floating-point numbers: This type contains numbers with a decimal point aushc as -12.43, 1.0 and 3.1414927.
  -- Double - double-precision floating-point numbers: This type is imilar to Float, expect that twice as much memory is used for storage of these numbers to increase their precision.

  -- List types: A list is a sequence of elements of the same type, with the elements being enclosed in square parentheses and separated by commas. We write [T] for the type of all lists whose elements have type `T`. 
[False, True, False] :: [Bool]
['a', 'b', 'c', 'd'] :: [Char]
["One", "Two", "Three"] :: [String]
  - The number of elements in a list is called its length. The list [] of length zero is called the empty list, while lists of length one, such as [False], ['a'] and [[]] are called singleton lists. 

  - Tuple types: A tuple is a finite sequence of components of possibly different types, with the components being enclosed in round parentheses and separated by commas `(T1, T2, ..., Tn)`. 
(False, True) :: (Bool, Bool)
(False, 'a', True) :: (Bool, Char, Bool)
("Yes, True, 'a') :: (String, Bool, Char)
 - The number of components in a tuple is called its arity. The tupple () of arity zero is called the empty tuple, tuples of arity two are called pairs, tuples of arity three are called triples and so on.
 - Tuples of arity one, such as (False) are not permitteed because they would conflict with the use of parentheses to make the evaluation order explicit, such as in (1 + 2) * 3. 
 - There are no restirctions on the types of the components of a tuple:

('a', (False, 'b') :: (Char, (Bool, Char))
(['a', 'b', [False, True]) :: ([Char], [Bool])
[('a', False), ('b', True)] :: [(Char, Bool)]
-}

-- Function types
{-
  - A function is a mapping from arguments of one type to results of another type. We write T1 -> T2 for the type of all functions that map arguments of type T1 to results of T2. For examples:
not :: Bool -> Bool
even :: Int -> Bool
-}

add :: (Int, Int) -> Int
add (x, y) = x + y

zeroto :: Int -> [Int]
zeroto n = [0..n]

{-
 - In the examples we have followed the Haskell convention of preceding function definitions by their types, which serves as useful documentation. Any such types provided manually by the users are checked of consistency with the types calculated automatically using type inference.
-}

-- Curried functions
{-
 - Functions are free to return functions as results.
 - The type states that `add'`is a function that takes an argument of type `Int` and returns a result of that is a function of type `Int -> Int`. `add'`takes an integer `x` and returns a function, which in turn takes an integer `y` and returns the result `x + y`
 - Functions with more than two arguments can also be handled using the same technique, by returning functions that return functions and so on.
 - The definition of mult states that `mult`takes an integer `x`and returns a function, which in turn takes an integer `y`and returns another function, which finally takes an integer `z` and returns the result `x * y * z` 

-}

add' :: Int -> (Int -> Int)
add' x y = x + y

-- mult :: Int -> Int -> Int -> Int // is the same just a convention in order not to use parentheses!!!
mult :: Int -> (Int -> (Int -> Int))
mult x y z = x * y * z

-- P 27
