# Haskell Book

[Source](http://haskellbook.com/assets/img/sample.pdf)

## Chapter 2

* What is prelude? Prelude is a library of standard functions. Prelude is contained in the base Haskell package. 

* If something or other is `in base`it means it's contained in that vast foundationla package.

* `:quit` `:q` is a GHCI command to exit the REPL

* `::` is the type signature. You can think of it as saying: has a type. So `sayHello` has the type `String -> IO()`

test.hs
```hs
sayHello :: String -> IO ()
sayHello x = putStrLn ("Hello, " ++ x ++ "!")
```

* in `stack ghci` RELP you can type `:load test.hs` and execute the function `sayHello()`

* `:m` will (:module) this will unload that `test.hs` from GHCi.

### Expressions

* Everything is Haskell is a expression or declaration. Expressions may be values, combinations of values, and/or a function applied to values. Expressions evaluate to results.

> Expressions are the building bloks of our programs, and programs themselves are one big expression made of smaller expressions. 

* Declarations are top-level bindings which allows us to name expressions. We can then use those names to refer to them multiple times without copying and pasting the expressions.

* The following all are expressions:

```hs
1 // has no further reduction step, so it stands for itself. It cannot be reduced any further

1 + 2 // GHCi reduces the expression 1 + 2 to 3, then prints the number 3. The reduction terminates with the number 3, because there are no more items to evaluate.

"Icarus"
```

* **Normal Form:** we say that expressions are in normal form when there are no more evaluation steps that can be taken. E.g. the normal form of `1 + 1` is `2` the `2` is no more reducible - it can't evaluate into anything other than itself.

* Reducible exprssions are also called redexes. While we will generally refer to this process as evaluation or reduction, you may also hear it called "normalizing" or "executing" and expressions.

## Functions

* Expressions are the most basic unit of a Haskell program, and functions are a spcial type of expressions. Functions map an input or set of inputs to an output. A function is an expressons that is applied to an argument and always returs a result. Because they are purely built of expressions, they will always evaluate to the same result when given the same values. 

* As in the lambda calculus, all functions in Haskell take one argument and return one result. 

> The way to think of this is that, in Haskell, when it seems we are passing multiple arguments to a function, we are actually applying a series of nested functions, each to one argument. This is called currying.

* Functions are how we factor out the pattern into something we can resuse with different inputs. You do that by naming the function and introducting an independent variable as the argument to the function. Functions can be used as arguments to functions, just as any other value can be.

```hs
triple x = x  * 3
```
* **Note:** After the equal sign there is an expression that is the body of the function and can be evaluated to return a value.

* Defining functions n a normal Haskell source code file and in GHCi are a little different

```hs
triple x = x * 3
```

in GHCi

```
Prelude > let triple x = x * 3
```

	1) `triple` is the name of the function, it is a function declaration.
	2) `x` is the parameter of the function. The parameters of our function correspond to the head of a lambda and bind variables that appear in the body expression.
	3) `=` is used to define (or declare) values and functions.
	4) `x * 3` this is the body of the function, an expression that could be evaluated  if the function is applied to a value. If `triple` is applied, the arguments it's applied to will be the value to which the x is bound. Here the expression `x * 3`constitutes the body of the function. So, if you have an expression like `triple 6`, x is bound to 6. since you've applied the function, you can also replace the fully applied function with its body and bound arguments `let result = 6 * 3`

* **Note:** Capitalization matters, you always needt to start with lowercase letters. 

### Evaluation

* Evaluating an expression means reducing the terms until the expression reaches it simpliest form. Once a term has reached its simplest form, we say that is irreducible of finished evaluation. It's called a value.

> Haskell uses a nonstrict evaluation ("lazy evaluation") strategy which defers evaluation of terms until they're forced by other terms referring to them. 

* Values are irreducible, but applications of functions to arguments are reducible. Reducing an expression means evaluation the terms until you're left with a value. Reducing an expression means evaluating the terms until you're left with a value. As in the lambda calculus, application is evaluation: applying a function to an argument allows evaluation or reduction.

* **Note:** Values are expressions, but cannot be reduced further. Values are terminal point of reduction. 

```hs
1
"Icarus"
```

* The following expressions can be reduced (evaluated) to a value. Each can be evaluation in the REPL, which reduces the expressions and then prints what it reduced to.

```hs
1 + 1
2 * 3 + 1
```

* Calling a function by name and applying it to an argument makes it a reducible expressions. In a pure functional languages like Haskell, we can replace applications of functions with their definitions and get the same result. 

* We know that since triple is defined as x = x * 3, the expressions is equalent to:

```hs
triple 2 
2 * 3
6
```

* We've applied `triple` to the value `2` and then reduce the expresssion to the final result `6`.

* **Note:** Haskell doesn't evaluate everything to canonical or normal form by default. Instead, it only evaluates to weak head normal form (WHNF) by default. In Haskell's nonstrict evaluation means not everything will get reduced to its irreducible form immediately.

```hs
(\f -> (1, 2 + f)) 2
```

reduces the following in WHNF:

```hs
(1, 2 + 2)
```

* This representation is an approximation ,but the key point here is that `2 + 2` is not evaluated to 4 until the last possible moment.

## Infix operators

```hs
id 1 
```
* **Note:** `id` is a built in function in Haskell such as currying also is built-in.

* Functions in Haskell default to prefix syntax, meaning that the function being applied is as the beginning of the expression rather than in the middle. 

* Operators are functions whcih can be used in infix style. All operators are functions, not all functions are opertors. While `triple` and `id`are prefix functions (not operators), the `+` function is an infix operator:

```hs
Prelude > 1 + 1
2
```

* If the function nam eis alphanumeric, it is a prefix function by default, and not all prefix functions can be made infix. If the name is a symbol, it is infix by default but can be made prefix by wrapping it in pranetheses.

```hs
Prelude> (+) 10 10
20
```

```hs
Prelude> :info (+)
```

* By typing `:info` and an opertor or a function you will get the type information.

## Declaring Values

* The order of declarations in a source code file doesn't matter because GHCi loads the entier file at once, so it knows all the values that have been defined.

* When you enter them one by one into REPL, the order does matter.

```hs
module Learn where
-- First we declare the name of our module so 
-- it can be imported by name in a project.
-- Remember: module names are capitalized
x = 10 * 5 + y
myResult = x * 5
y = 10
```

* You can import a module

```hs
import Learn

sayHello :: String -> IO ()
sayHello x = putStrLn ("Hello, " ++ x ++ "!")
triple :: Int -> Int
triple x = x * 3
```

## Troubleshooting

* One thing to keep in mind is that identation of Haskell code is significant and can change the meaning of the code. Incorrect identation of code can also break your code. **Reminder:** use spaces, not tabs, to indent your source code.

* In source code files, identation often replaces syntactic markers like curly brackets, semiconons, and parentheses. The basic rule is that code that is part of an expression should be indented under the beginning of that expression, even when the beginning of the expression is not at the leftmost margin. 

> Syntax is the grammer and structure of the text we use to express programs, and syntactic sugar is a menas for us to make that tet easier to read and write. Syntatic sugar can make the typing or reading of code nicer but changes nothing about the semantics, or meaning of programs and doesn#t change how we solve problems in code. Typically when code with syntactic sugar is processed by our REPL or compiler, a simple transformation from the shorter ("sweeter") form to amore verbose, truer representation is perfored after the code has been parsed. 

## Prenthesization

* `$`will allow everything to the right of it to be evaluated first and can be used to delay function application.

```hs
Prelude > (^2) $ (+2) $ 3 * 2
64
```

```hs
-- Remember ($)'s defintion, you can 
f $ a = f a
```

## Let and where

* `let` and `where` used to introduce components of expression, and they seem similar. The contrast here is that `let` introduces an expression, so it can be used whenever you can have an expression, but `where´ is a declaration and is bound to a surrounding syntactic construct.

* Scope is the area of source code where a binding of a variable applies. 

```hs
mult1    = x * y
 where x = 5
       y = 6
```

* Making the equals signs line up is a stylistic choice. As long as things are nested in that way the equals signs do not have to line up. 

# Strings Chapter 3

* A string is a list of characters `Hello === ['H', 'e', 'l', 'l', 'o']

## Types?

* Types are a way of categorizing values. There are several types for numbers, for example, depending on whether they are integers, franctional numbers. There is a type for boolean values (true/false). The types we are primarily concerned with in this chaper are `Char` (characters) and `String`. Strings are lists of characters.

* To find a type we can type `Prelude> :type: 'a'` will get `'a' :: Char`

* Here in the example above we have enclosed the character in single quotes. This lets GHCi know that the character is not a varibale. If you enter `:type a`instead, it will think it's a variable and give you an error message that the `a` is not in scope. That is, the variable a hasn't been defined (is not in scope), so it has no way to know what the type of it is. 

* The `::` symbol is read as `has the type`. Whenever you see that double colon, you know you're looking at a type signature. A type signature is a line of code that defines the types for a value, expression, or function.

* `Char` is the type that includes alphabetic characters, unicode characters, symbols etc. So asking GHCi `:type 'a'`that is what is the type of 'a'? gives us the information `'a' :: Char`that is 'a' has the type of Char.

* If we have a string we need to use `"Hello World"`double quotation marks, not single, to tell GHCi w have a string, not a single character. 

```
Prelude> : type "Hello!"
"Hello!" :: [Char]
```

* There is something new in the type information. The square brackets around `Char`here are the syntactic sugar for a list. `String` is a type aliases, or type synonym for a list of `Char`. 

* A type alias means we use one name for a type, usually for convenience, that has a different type name underneath. `String` is anohter name for a list of characters. By using the name `String`we are able to visually differentiate it from other types of lists.

```
Prelude> print "hello world!"
"hello world!"
```

* There are also other commands we can use to tell GHCi to print strings of text into the display

```
Prelude> putStrLn "hello world!"
hello world
Prelude>

Prelude> putStr "hello world!"
hello world!Prelude>
```
* You can write it in a file and load into GHCi

```hs
module Print1 where

main :: IO()
main = putStrLn "Hello World!"
```

* **Note:** you can use `:l moduleName.hs`to load the module or `:m` to unload the module, to go back to `Prelude>`

* **Impotant:** As you can see, `main` has the type `IO()`. IO stands fro input/output but has a psecialized meaning in Haskell. It is a special type used when the result of running the programm involves effects in addition to being a functon or expression. Printing to the screen is an effect, so printing the output of a module must be wrapped in this IO type. 

```hs
module Print2 where 
main :: IO()
main = do 
 putStrLn "Count to four for me:"
 putStr "one, two"
 putStr "three, and"
 putStrLn " four!"
```

* The `do`syntax is a special syntax that allows for sequencing actions. It is most commonly used to sequence the actions that constitute your program, some of which will necessarily perform effects such as printing to the screen (that's why the obligatory type of `main`is IO()). The `do`isn't strictly necesary, but since it foten makes for more readable code than the alternatives, you'll see it a lot. 

* **Note:** `Ln` indicates that it starts a new line where `putStr` doesn't starts a new line.

> To concatenate something means to link together. Usually when we talk about concatenation in programming we're talking about linear sequences such as lists or strings of text. 

```hs
module Print3 where

myGreeting :: String
-- the above line reads as: "myGreeting has the type of String"

myGreeting = "hello" ++ "world"

hello :: String
hello = "hello"

world :: String
world = "world!"

main :: IO()
main = do
 putStrLn myGreeting
 putStrLn secondGreeting
 where secondGreeting = concat [hello, " ", world]
```

## Top-level versus local definitions

* Top-level declarations are not nested within anything else and they are in scope throughout a local definition. 

* Local definitions would mean the declaration is nested within some other expression and is not visible outside that expression. 

```hs
module Nested where

topLevelFunction :: Integer -> Integer
topLevelFunction x = x + woot + topLevelValue
 where  woot :: Integer
        woot = 10

topLevelValue :: Integer
topLevelValue = 5
```

* In the above you could import and use topLevelFunction or topLevelValue from another module, and they are accessible to everything else in the module. However, `woot`is effectively invisible outside of `topLevelFunction`. The `where`and `let` clauses in haskell introduce local bindings or declarations. **To bind or declare something means to give an expression a name.** You could pass arond and use an anonymous version of `topLevelFunction`manually, but giing it a name and reusing it by that name is less repetitious.

* The `++`function is an infix operator. When we need to refer to an infix operator in a position that is not infix - such as when we are using it in a prefix position or having it stand alone in order to query its type - we must put parentheses around it. 

```hs
con = (++) "hello" "world"
``` 

* They type of `concat` says that we have a list of lists as input and we will return a list. 

```
Prelude> :t (++)
(++) :: [a] -> [a] -> [a]

Prelude> :t concat
concat :: [[a]] -> [a]
```

* It will have the same values inside it as the list of lists did; it just falttens it into one list structure, in a manner of speaking. A `String` is a list, a list of `Char` specifically, and `concat` can work on lists of strings or lists of lists of other things.

```
Prelude> concat [[1, 2, 3], [4, 5, 6]]
[1,2,3,4,5,6]
Prelude> concat ["Hello", "World!"]
"HelloWorld!"
Prelude> (++) [1, 2, 3] [4, 5, 6]
[1,2,3,4,5,6]
```

```
Prelude> :type concat
concat :: Foldable t => t [a] -> [a]
```

```
Prelude> :type (++)
(++) :: [a] -> [a] -> [a]
	(1)    (2)    (3)
```

* Everything after the `::`is about our types, not our values. The 'a' inside the list type constructor [] is a type variable.

1) Take an argment of type `[a]`. This type is a list of elements of some type `a`. This function does not know what type 'a' is. It doesn't need to know. In the context of the program, the type of `a` will be known and made concrete at some point.

2) Take another argument of type `[a]`, a list of elements whose type we don't know. Because the variables are the same, they must be the same type throughout (a == a)

3) Return a result of type `[a]


P.53
