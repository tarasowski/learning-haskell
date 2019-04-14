module Part1 where

sayHello :: String -> IO()
sayHello x = putStrLn("Hello, " ++ x ++ "!")

{-
- In Haskell, a function is a mapping that takes one or more arguments and produces a single result.

- Functional programming can be viewed as a style of programming (paradigm) in which the basic method of computation is the application of function to arguments. In turn, a functional programming language is one that supports and ecnourages the functional style.
-}

{-
- In general, programming languages such as Java in which the basic method of computation is changing stored values are called imperative languages, because programs in such languages are constructed from imperative instructions that specify precisely how the computation should proceed.
int total = 0;
for (int count = 1; count <= n; count++)
   total = total + count;

total = 0;
count = 1;
total = 1;
count = 2;
total = 3;
count = 3;
total = 6;
count = 4;
total = 10;
count = 5;
total = 15;
-}

{-
 - Lets do the same example in Haskell. This would normally be achieved using two libraray functions, one called [..] that is used to produce the list of numbers, and the other caled sum that is used to produce the sum of this list: sum [1..n]
 - In this program, the basic method of computation is applying functions to arguments, in the sense that executing the program results in a sequence of application.
 
sum [1..5]
=   {applying [..]}
    sum [1, 2, 3, 4, 5]
=   {applying sum}
    1 + 2 + 3 + 4 + 5
=   {applying + }
15

 - In Haskell, every function has a type that specificies the nature of its arguments and results, which is automatically infelred (abgeleitet) from the definition of the function. For example, the function sum defined above has the following type:

Num a => [a] -> a
 - This type states that for any type `a` of numbers, `sum` is a function that maps a list of such numbers to a single such number 
 - Types provide useful information about the nature of functions, but more importantly, their use allows many errors in programs to be automatically detected prior to executing the programs themselves.
 - In particular, for every occurence of function application in a program, a check is made that the type of the actual arguments is compatible with the type of the function itself.

-}

addm :: [Int] -> Int
addm (x:xs) = sum(x:xs) 

{-
 - In Haskell, sum can be defined using two equations:
 sum [] = 0
 sum (n:ns) = n + sum ns

 - The first equation states that the sum of the empty list is zero, while the second states that the sum of any non-empty list comprising a first number `n` and a remaining list of numbers `ns`is given by adding `n` and the sum of `ns`. The result of sum [1, 2, 3] can be calculated as follows
 -
sum [1, 2, 3]
= {applying sum}
1 + sum [2, 3]
= {applying sum}
1 + (2 + sum [3])
= {applying sum}
1 + (2 + (3 + sum [])
= {appyling sum}
1 + (2 + (3 + 0))
= {appyling + }
6

 - Note that even though the function `sum`is defined in terms of itself and is hence `recursive`, it does not loop forever. In particular, each application of `sum` reduces the length of the argument list by one, unit the list eventually becomes empty, at which point the recursion stops and the additions are performed.
 - Returning zero as the sum of the empty list is appropriate because zero is the identity for addition. That is, 0 + x = x and x + 0 = x for any number of x. 
 
-}

mySum (n:ns) = n + sum(ns)

{--
 - Sotring values: Suppose that we define a function called qsort by the following two equations
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
               where
                  smaller =   [a | a <- xs, a <= x]
                  larger  =   [b | b <- xs, b > x]
 - In this defintion, `++`is an operator that appends two lists together; for example, [1, 2, 3] ++ [4, 5] = [1, 2, 3, 4, 5] 
 - [1..5] ++ [6, 7] = [1, 2, 3, 4,5, 6, 7]
 - `where` is a keyword that introduces local definitions, in this case a list `smaller`comprising all elements `a`from the list `xs` that are less than or qual to x, together with a list `larger` comrising all elements `b` from `xs` that are greater than x. For example, if x = 3 and xs = [5, 1, 4, 2], then smaller = [1, 2] and larger = [5, 4]. 
--} 
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
                where
                  smaller = [a | a <- xs, a <= x]
                  larger  = [b | b <- xs, b > x]

{--
-  What does qsort actually do? First of all, we note that it has no effect on list with a single elements, in the sense that qsort [x] = [x] for any `x`. It's easy to verify this property using a simple calculation:
qsort [x]
= {applying qsort}
qsort [] ++ [x] ++ qsort []
= {applying qsort}
[] ++ [x] ++ []
= [x]
--} 

{--
qsort [3, 5, 1, 4, 2]
= {applying qsort}
qsort [1, 2] ++ [3] ++ qsort [5, 4]
= {appyling qsort}
(qsort [] ++ [1] ++ qsort [2]) ++ [3] ++ (qsort [4] ++ [5] ++ qsort [])
= {applying qsort, above property}
([] ++ [1] ++ [2]) ++ [3] ++ ([4] ++ [5] ++ [])
= {applying ++}
[1, 2] ++ [3] ++ [4, 5]
= {appyling ++}
[1, 2, 3, 4, 5]
 - The first equastion for `qsort` states that the empty list is already sorted, while the second states that any non-empty list can be sorted by inserting the first number between the two lists that result form sorting the remaining numbers that are smaller and larger than this number. This method is called `quicksort`
--}

{--
 - The function qsort is also more general not just with numbers, but with any type of ordered values.
qsort :: Ord a => [a] -> [a]
 - It states that, for any type of `a` of ordered values, qsort is a function that maps between lists of such values. The function qsort could be also used to sort could also be used to sort a list of characters, or a list of strings
--} 

{--
 - Consider a function called seqn that takes a list of input/output actions, such as reading or writing a single character, performs each of these actions in sequence.
--} 
seqn [] = return []
seqn (act:acts) = do x <- act
                     xs <- seqn acts
                     return (x:xs)
{--
 - These two equations state that if the list of actions is empty we return the empty list of results, otherwise we perform the first action in the list, then perform the remaining actions in the list, and finally return the list of results that were produced. For example, the expression seqn [getChart, getCahr, getChar] reads three characters from the keyboard using the action `getChar`that reads a single character, and returns a list of containing the three characters.
 - The interesting aspect of the function `seqn`is its type. One possible type that can inferred from the above defintions is the following:
seqn :: [IO a] -> IO [a]

- This type states that `seqn`maps a list of `IO` (input/output) actions that produce results of some type `a`to a single IO action that produces a list of such results. More importantly, however, the type also makes explicit that the function `seqn`involves the side effect of performing input/output actions. 
- Using types in this manner keep a clear distinction between functions that are pure and those that involve side effects is a central aspect of Haskell, and brings **important benefits in terms of both programming and reasoning.**
--}
double :: Num a => a  -> a
double (x) = x + x

concatList :: Num a => [a] -> [a]
concatList (xs) = xs ++ [1, 2, 3, 6]

{-
 - In Haskell all computations are done via the evaluation of expressions (syntactic terms) to yield values (abstract entities that we regard as answers).
 - Every value has an associated `type`. We can think of types as a set of values. Examples of expressions include atomic values such as integer 5, the character 'a', and the function \x -> x + 1, as well as structured values such as the list [1,2,3] and the pair ('b', 4). 
 - Since functions are values in Haskell. Functions have also types. See the function `inc`

5     :: Integer
'a'   :: Char
inc   :: Integer -> Integer
[1,2,3] :: [Integer]
('b', 4) :: (Char, Integer)

 - `::`can be read "has type".
 - Functions are normally defined by a series of equasionts. The function `inc` can be defined by the single equation:
-}
inc n = n + 1 

myProduct [] = 1
myProduct (n:ns) = n * product ns

qsortR [] = []
qsortR (x:xs) = qsortR larger  ++ [x] ++ qsortR smaller
                where
                  smaller  = [a | a <- xs, a <= x]
                  larger  = [b | b  <- xs, b > x]

qsortE [] = []
qsortE (x:xs) = qsortE smaller ++ [x] ++ qsortE larger
                where
                  smaller = [a | a <- xs, a < x]
                  larger = [b | b <- xs, b > x]

