square x = x * x
-- No parentheses around parameters
-- No return keyword
-- All functions in Haskell are built by combining small functions together

posOrNeg :: Int -> [Char]
posOrNeg x =
  if x >= 0
    then "Positive"
    else "Negative"
-- No parens around condition
-- No return statement


-- Pure Functions
  -- All Haskell functions are pure function.
  -- A pure functionc calls other functions to compute a value and return that value
  -- Pure function cannot modify variables, writing to command line
  -- Pure function cannot depend on state (cannot read a file or get user input)
  -- Pure functions only depend on its arguments

-- Pure Function Examples
-- Print a string to the console: not pure, becuase it modifies external state. What appers on the console
-- Read a file: not pure, depends on external state at different times
-- Compute the length of string: pure - no state, the return value only depends on the args
-- Get the current time: not pure - returns different values when called at different times (depends on a state of clock)
-- Get a random number: not pure - it returns different values each time it's called. It depends on some sort of external state.
-- You can do all the impure stuff in Haskell, you JUST CANNOT USE FUNCTIONS.


-- Recursion
-- A recurisve function is a function that call itself.
pow2 n =
  if n == 0
    then 1 -- base case of recursion
    else 2 * (pow2 (n-1)) 

repeatString str n =
  if n == 0
    then ""
    else str ++ (repeatString str (n-1))

-- Every loop can be rewritten as a recursive functon

-- Lists
-- Haskell has Arrays. But Lists are prefered

x = [1, 2, 3]
empty = []
y = 0 : x -- prepend an element to a list
-- : is a cons operator it add the element to a a list in the front and returns a new list. It does not modify the list x.

-- x = [1, 2, 3] is just a short hand for the below notation
x' = 1 : (2 : (3 : []))
x'' = 1 : 2 : 3 : []

-- Strings
-- are list of Characters
-- ++ creates a new list with elements by the first list followed by the second list
-- Lists in Haskell must be Homogenious = they need to contain the same type error = [1, "hello", 2]

str' :: [Char]
str' = 'a' : 'b' : 'c' : 'd' : 'e' : []

str :: [Char]
str = "abcde"

strC :: [Char]
strC = ('a' : ('b' : [])) ++ ('c' : 'd' : 'e' : [])

-- List Functions
h = head str
t = tail str
mSnd = head (tail [1, 2, 3, 4])

ch :: Bool
ch = null [] -- True

ch' :: Bool
ch' = null [1, 2] -- False

-- We use recursion here, because we want to double every element in the list
double nums =
  if null nums
    then []
    else (2 * (head nums)) : (double (tail nums))


-- Tuples
-- Provide a conviniet way to package values together, so they can be passed around togther

t' = (1, "hello")

-- List vs. Tuples
-- same type vs. different types
-- unbounded length vs. fixed length

-- To return multiple values from a function you can use touples
headAndLength list = (head list, length list)

-- Accessing Tuple Elements

accf = fst (1, "hello")
accs = snd (1, "hello")

-- Tuple Warning: 
-- Big tuples not good
-- Tuples spanning different parts of an application not good

-- Pattern Matching
-- Is a basic tool to access data in composite data structure like lists and tuples.

-- Simplest example of pattern matching
fst' (a,b) = a
snd' (a,b) = b

-- This is how null is implemented
null' [] = True -- first pattern: when the list is empty it returns True
null' (x : xs) = False -- second pattern: when the list is not empty return False

head' (x : xs) = x
-- head' [] = ?? 
-- head' [] = error "head of empty list" -- it will cause the program to crash, but another way to match the pattern
{-
 - Without pattern matching
 double nums =
  if null nums
    then []
    else (2 * (head nums)) : (double (tail nums))
-}
-- With pattern matching
double' [] = []
-- (x : xs) means the first element is x and xs are all other elements
double' (x : xs) = (2 * x) : (double xs)

-- Guards
-- Pattern matching looks a the structure of the data.
-- Guards can looks at the values in the data

pow2' n 
  | n == 0 = 1
  | otherwise = 2 * (pow2' (n-1))

-- | (pipe) before each guard followed by a guradian boolean expression followed by the = sign
-- otherwise is a catch-all case (_). It's just a keyword


-- Case Expressions
double'' nums = case nums of
  [] -> []
  (x : xs) -> (2 * x) : (double xs)

-- in the function above the pattern is [] and (x : xs)

lEmpty nums = case (tail nums) of
  [] -> False
  (x : xs) -> True


-- Let Binding
-- Let Binding define a local variable. Those local variables cannot be changed
-- Local variables will always have the same value when they have been created
-- keyword `in` marks a sub expression where the variable bound to `a` can be used

fancySeven = 
  let a = 3
  in 2 * a + 1

countA nums = 
  let a = (tail nums)
  in length a

-- Where Binding
-- This is another way of binding a local variable. 
-- Where binding comes after the function which uses the variables


fancySeven' = 2 * a + 1
  where a = 3

-- Like let, also where can bind multiple variables
fancySeven'' = y + x
  where y = 3
        x = 4

-- Let expressions can be used within a function defintion. While where is not allowed to use within function defintion
-- Where - top down
-- Let - bottom up

-- Whitespace in Haskell has significant meaning
-- Don't put tabs in your source code (insert spaces for tabs/indenting)
-- Identation matters!!!
{-
 - Produces an error
 fancyNine =
  let x = 4
  y = 3
  in x + y

 - No errors
 fancyNine =
  let x = 4
      y = 3
  in x + y
-}

-- Lazy Function Evaluation
-- In most programming languages there is order of execution 
-- foo(alpha(1), beta(2)) - in this example we know that alpha(1) and beta(2) are first computed and then the resulting values are passed to foo, which is then computed to get a return value
-- In Haskell there is no such a guarantee
-- foo (alpha 1) (beta 2) - a function can return without computing (alpha 1)

-- Lazy Infinite Lists
-- This won't crash. It does not compute anything until it's needed by our program
intsFrom n = n : (intsFrom (n + 1))
ints = intsFrom 1
empty' = null ints -- False
head'' = head (ints) -- 1

-- We are able to evaluate parts of an infinite list without any problems
take'' = take 10 (ints)

-- HASKELL IS SO LAZY THAT IT JUST EVALUATES TO GET ANSWER AND NO MORE
-- to get the first 10 elements of ints, it's only evaluates the first 10 elements and stops
-- (length ints) will never return anything it will just run and try to evaluate

