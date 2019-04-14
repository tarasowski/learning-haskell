module Part2 where 
{-
 - Glasgow Haskell Compiler is open source implementation of Haskell. The system has two main components: a batch compiler called GHC, and an interactive interperter called GHCi. We use here the GHCi, but if stand-alone executable version of Haskell program is required, the compiler itself can be used.
 - Haskell comes with a large number of built-in functions, which are defined in a library file called standard prelude. Prelude provides a range of functions that operate on lists. In Haskell, the elements of a list are enclosed by square parentheses and are separated by commas, as in [1, 2, 3, 4, 5]
-} 

-- Working with lists in Haskell

getFirst xs = head xs
removeFirst xs = tail xs
selectSpecificElement n xs = xs !! n
selectFirstElements n xs = take n xs
removeFirstElements n xs = drop n xs
calculateLength xs = length xs
calculateSum xs = sum xs
calculateProduct xs = product xs
appendLists xsa xsb = xsa ++ xsb
reverseList xs = reverse xs

-- Function application
{-
 - In math, the application of a function to its arguments is usually denoted by enclosing the arguments in parenthese, while the mutliplication of two values is often denoted silently, by writing two values next to one anohter.
f(a, b) + c d 
 - Means apply the function to two arguments `a` and `b`and add the result to the product of `c` and `d`. In Haskell the function application is denoted silently using spaces, while the multiplication of two values is denoted explicitly using the operator *. For example the expression above would be written in Haskell `f a b + c * d`. Moreover function application has a higher prioerity than all other opertors in the language.
Math:
f(x)
f(x, y)
f(g(x))
f(x, g(y))
f(x)g(y)

Haskell:
f x
f x y
f (g x)
f x (g y)
f x * g y

- Note the parentheses are still required in the Haskell expression f (g x), because f g x on its own would be interpreted as the application of the function `f` to two arguments `g` and `x`. Where the intention is that `f` is applied to one argument, namely the result of appyling the function `g` to an argument `x`.  
-}

-- Haskell Scripts
{-
 - New functions can be defined in a script, a text file comprising (beinhaltet) a sequence of definitions. 
 - By convention, Haskell scripts usually have a `.hs` suffix on their filename to differentiate them for other kinds of files. **This is not mandatory**
 - You can type `stack ghci` so load the interactive compiler. If you want to load a specific file type `:l filename.hs` if you have changed something in the file type `:r` to realod the file.
-}

double x = x + x
quadruple x = double (double x)

extractList x xs = take (double x) xs

getFactorial n = product [1..n]

-- We could also hae defined average ns = div (sum ns) (length ns), but writing div between its two arguments is more natural. 
-- In general, any function with two arguments can be written between its arguments by enclosing the name of hte function in single back quotes ``. 
getAverage ns = sum ns `div` length ns

{-
 - You can type `:type expr` in the GHCi to show type of expression. A good way to learn how the types of functions are constructed. 
-}

-- Naming requirements
{-
 - When defining a new function, the names of the function and its arguments must begin with a lower-case letter, but can then be followed by zero or more letters, digits, underscores, and forward single quotes.
 - The following list of keywords have a special meaning and cannot be used as the names of functions or their arguments: case, class, data default, deriving, do else, foreign, if, import, in, infix, infixl, infixr, instace, let, module, newtype, of, then, type, where
 - ** By convention, list arguments in Haskell have the suffix `s` on their name to indicate that they may contain multiple values. For example: a list of numbers might be named `ns`, a list of arbitrary values might be named `xs`, and list of lists of characters might be named `css`
-}

-- The layout rule
{-
 - Within a script, each definition at the same level must begin in precisely the same column. 
-}

a = b + c
    where
      b = 1
      c = 2
d = a * 2

{-
 - It's clear that `b` and `c`are local definitions for use within the body of `a`. If desired, such grouping can be made explicit by enclosing a sequence of definitions in curly parentheses and separating each definition by a semicolon.
 - Tabs: Tab characters can cause problems in scripts, because layout is significant but different text editors interpert tabs in different ways. For this reason it's recommended to avoid using tabs -> configure your editor to automatically convert them to spaces.
-}

c = a + b
    where
      {b = 1;
      c = 2};
g = c * 2

n = a `div` length xs
    where
      a = 10
      xs = [1, 2, 3, 4, 5]

myLast xs = head (reverse xs)
removeLast xs = reverse (tail (reverse xs))
removeLastLenght xs = take ((length xs) - 1) xs
removeLastWhere xs = take n xs
                   where 
                    l = length xs
                    n = l - 1
