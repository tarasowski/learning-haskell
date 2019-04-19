module IO where

-- IO actions in Haskell
-- IO is Haskells way to interact with programs from the outside world.
-- And do things without sacrificing function purity
-- All Haskell functions are pure functions, meaning compute it's results based on the arguments.
--    They cannot modify external state
--    The value cannot depend on external state that might possibly change
--    Cannot write to the console (cannot do effects)
-- Haskell is stubborn and forbids you to write functions with side-effects
-- So what is putStrLn function when it's not allowed to do side-effects? It's an IO action.


-- IO Actions
-- -> means a function type
-- putStrLn :: String -> IO ()
-- means a function takes a String and returns something???

-- () are a type called Unit in Haskell. Unit is more or less equivalent to void in other Languages.
-- it's just special notation for this algebraic data type: data Unit = Unit
-- that is Unit is a type with only one value also called Unit
-- Unit can't convey any information since it has always the same value.
-- Unit is used as a placeholder containing no data at all.

-- What's about IO?
-- It's parametirized type, meaning with a type variable slot that can hold any other type.
-- data IO a
-- putStrLn :: String -> IO ()
-- The putStrLn function takes a String and returns a value of type IO with the Unit type as the parameter.
-- In other words it returns IO Unit

-- What is an IO Unit == IO ()?
-- An IO unit is an IO action that represents some code that could be run and interact with the outside world.
-- In cas eof putStrLn represents a funtion that can be run and print the String to the console.
-- But that's all it is. Code that could be run. It isn't actually run when we call a putStrLn function. 
-- You can call the putStrLn function all over the program, but nothing will happen. 
-- Unless you use the IO action in main. 
-- Since the main variable is the value return from putStrLn line, we know main has type IO Unit main :: IO ()
-- So main in itself is an IO action, but it's special. The Haskell runtime looks for variable named `main` which must have type IO Unit. The IO action contained in the `main` variable is actually executed and any side-effects is has, actually happen. In this case the String "Hello World!" is printed to the console.
-- main - IO action executed by the program. 


-- do-Blocks
-- So far you know how to make a program to make 1 thing e.g. print a string
-- If you want to print two strings you can use a do block to chain serveral action together into a single IO action
-- All you need is the `do` keyword followed by several IO actions by separate indented lines
-- The IO actions in the `do` block will run 1 after another

helloWorld :: IO ()
helloWorld = putStrLn "Hello World!"

-- IO actions can be composed using do blocks
-- This is one composite IO action, we're using a do block to compose both putStrLn
-- The introduce function dosn't do any effects. It return an IO action. The IO actions gets executed by the runtime.
introduce :: String -> String -> IO ()
introduce name1 name2 = do
  putStrLn (name1 ++ ", this is " ++ name2)
  putStrLn (name2 ++ ", this is " ++ name1)


-- IO Values
-- With IO actions you know know how to influence the outside world. But what's about reverse? 
-- How the program can get data about the outside world?
-- IO handles this as well. 
-- getLine :: IO String
-- As said earlier IO is a parametarized data type. putStrLn uses Unit as a type paramater, but getLine uses String as a type parameter.
-- The type parameter for IO is the type produces by the IO action. 
-- In other words. getLine :: IO String contains some instructions that when executed can both examine and modify the state outside world and return a String
-- In particular the instruction of getLine IO actions will read characters from the console buffer and remove them so that later call don't just keep returning the same characters.
-- Once we have the IO String we need somehow to turn it into an ordinary String, so we can use it in our code.
-- This is where the (<-) left arrow comes in. The right side of the arrow is the IO action. The lft side of the arrow is the variable name, which will refer to the value produced by that IO action. 
-- The arrow is sets to bind the resuls of IO action to the variable name
-- (<-) ONLY inside do-block
-- And bound variable line can only be used later in the same do-block
-- This means you can extract the value of an IO action inside another IO action. 
-- Also keep in mind that do blocks create IO actions that can be executed later. So the <- doesn't extract the value right away. What it really does is say: for the remainder of this do-Block the line refers to the value produced by getLine whatever that turns out to be. 
-- When the IO action is executed the value of line will be computed and then used everywhere, where the `line` variable appears. 
-- Just as with IO action you can compose also the IO values together

greet :: IO ()
greet = do
  putStrLn "Who are you?"
  who <- getLine
  putStrLn ("Hello " ++ who)

greetForever :: IO ()
greetForever = do
  greet
  greetForever --recursively. It's lazy, so at every step Haskell evaluates only the information that is needed. 
  -- this pattern calling IO actions recursively is the common way to achieve looping within IO actions.

-- Haskell Beginner was to use a function that extracts the value out of IO.
-- extractValue :: IO a -> a -- this doesn't work. You cannot use binding arrows to achieve such an action.
-- Such a function cannot fundamentaly exist, since it has to execute the IO action to get the value. Which would have side-effect. Which means the extractValue function by itself would have side-effects.
-- There is only 1 special exception to this: There is a function called unsafePerformIO that extract IO value like this. You shouldn't use it ever...


-- return Function
-- return is just a function that takes a value and produces IO action that when executed does nothing and immediately gives back that value.
-- If the point of IO to create side-effects. Why don't you have the point of IO that does nothing?
-- The return function can be very helpful when combining togehter multiple values.

dummyGetLine :: IO String
dummyGetLine =
  return "I'm not really doing anything" -- return :: a -> IO a

-- This program has an IO action names proptInfo. The type of value produced by that action when is executed is a pair of String (String, String)
-- Which will correspond to persons name and favorite color
promptInfo :: IO (String, String)
promptInfo = do
  putStrLn  "What is your name?"
  name <- getLine -- <- makes name to become ordinary String not an IO value
  putStrLn "What is your favorite color?"
  color <- getLine -- <- makes color to become ordinary String not an IO value
  -- Everything in the IO block has to be an IO action. We need to turn the pair of String into an IO (String, Strin)
  -- To do so we can use return function
  -- return :: Monad m => a => m a
  return (name, color) -- since it's the last action in the do block it's value becomes produces by the big IO action representing the entire do block. 

-- simple program
readM :: IO (String, String)
readM = do
  line1 <- getLine
  line2 <- getLine
  return (line1, line2)

-- return function doesn't terminate the function, doesn't return from a function. Return is not a keyword or a control flow construct like in any other languages.
-- return is just a function that builds an IO action
-- return :: Monad m => a -> m a


main :: IO ()
-- Another composite IO action, using do block to compose all other IO actions into one.
main = do
  -- this is pattern matching with the binding arrow
  -- the pair of Strings produces by the promptInfo action, is immediately deconstructed by this pattern to access the name and color.
  (name, color) <- promptInfo
  (line1, line2) <- readM
  -- to bind the variable inside the do-Block you need to use the let binding
  let lines = line1 ++ line2
  putStrLn lines
  putStrLn ("Hello " ++ name)
  putStrLn ("I like " ++ color ++ " too!")
  line' <- dummyGetLine
  putStrLn line'
  --greetForever
  line <- getLine -- getLine :: IO String
  putStrLn ("You said: " ++ line)
  putStrLn "Hello" -- :: String -> IO ()
  putStrLn "World"
  helloWorld
  helloWorld
  helloWorld
  introduce "Anastasia" "Dimitri"
  introduce "Mike" "Bob"
x = 3 -- A `do` block continues as long as the lines are indented and it ends on the first line that isn't indented
