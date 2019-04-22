module IO where

{-
 - In Haskell, an interactive program is viewed as a pure function that:
 -  takes a current state of the world as it's arguments
 -  and produces a modified world as it's result,
 -  in which the modified world reflects any side-effects that were performed by the program during its execution.
 - The notion of interactive program can be represented by a function of type World -> World, which we abbriviate as IO
 -  type IO = World -> World
 - In general an interactive program can return a result value in addition to performing side-effects. 
 - For example, a program for reading a character from the keyboard may return the character that was read.
 - For this reason, we generalise our type for interactive programs to also return value, with the type of such values being a parameter for the IO type:
 -  type IO a = World -> (a, World)
 
 - Expressions of type `IO a` are called actions. For example `IO Char` is the type of actions that return character, while `IO ()` is the type of actions that return the empty tuple () as a dummy result value. Actions of the latter type can be tought of as purely side-effecting actions that return no result value.
-}


main = do
  putStrLn "Hello World"

act :: IO (Char, Char)
act = do 
  x <- getChar
  getChar
  y <- getChar
  return (x, y)

getLine' :: IO String
getLine' = do
  x <- getChar
  if x == '\n' then
    return []
  else
    do xs <- getLine'
       return (x:xs)

putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x:xs) = do
  putChar x
  putStr' xs

putStrLn' :: String -> IO ()
putStrLn' xs = do
  putStr xs
  putChar '\n'

strLen :: IO ()
strLen = do
  putStr' "Enter a string: "
  xs <- getLine'
  putStr' "The String has "
  putStr' (show (length xs))
  putStrLn' " characters"
