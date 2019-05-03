module Mc where

import Control.Monad

-- Monadic flow control: they mimic familiar flow control concepts in other languages.
-- Turn an ordinary function into a monadic function

-- import Control.Monad
-- when working with Monads like State or IO which are related to imperative programming.
-- It's helpfull to use things like foor loops, but in Haskell those are just functions nothing special.
-- it's analogue to forEach in some languages
-- forM :: Monad m => [a] -> (a -> m b) -> m [b] -- it takes a list of values of type `a` and a function that accepts and item from the list, and returns some action in the monad producting a value of some other type `b`. forM returns a monad action producing a list of the values produced by each iteration of the loop. 
-- forM_ :: Monad m => [a] -> (a -> m b) -> m () -- this function throws the result away and retuns just a monadic action by producing a unit.
-- when :: Monad m => Bool -> m () -> () -- if you want to execute something if something is true. This is usually handled with the if construct. But Haskell's functional if construct must always have an else clause. Which is not always what you want in an imperative style monadic code. The when function takes a boolean value and a monadic action, which is executed if the boolean is true. It returns the monadic action which is either the given monadic action if the boolean was true, or is an empty action that doesn't do anything if the boolean is false.

debug = True


-- Lifting: liftM
-- Often you have an ordinary function but want to apply it to monadic values. 
-- You can always use the bind or do notation to extract the value, apply the function and package the result back up using the `return` function. But there is a better way!!!
-- liftM :: Monad m => (a -> b) -> (m a -> m b) -- takes a function from type `a` to type `b`. And returns a function which takes a monadic action of a value of type `a` and returns a monadic action of type `b`.
-- liftM2 :: Monad m => (a1 -> a2 -> b) -> m a1 -> m a2 -> m b
justFour = liftM (1+) (Just 3)
justFive = liftM2 (+) (Just 3) (Just 2)
printElem = mapM print [1,2,3,4]

-- IMPORTANT: Whenever you find yourself to use do notation to unpack the function, modify the results and pack them back up again with return. You can usually refactor the code into someting much shorter using liftM or one of it's variants.

-- Monadic List Functions: mapM
-- mapM is the monadic version of a map function.
-- mapM :: Monad m => (a -> m b) -> [a] -> m [b] -- it takes a function that takes a value of type `a` and returns a monadic action of producing the value of type `b`. It takes also a list of values [a] and returns a monadic action that execute the monadic action from applying the function to each of the values in the list and it then produces a list of the results.

-- Monadic List Functions: filterM
-- filterM :: Monad m => (a -> m Bool) -> [a] -> m [a] -- it takes a predicate, that takes an element and returns a monadic action to determine whether the element should be kept or not. The second argument is a list of arguments that will be tested by the predicate. It returns a monadic action that tests each element in the list and produces a list of the elements which pass.

askToKeep :: Int -> IO Bool
askToKeep x = do
  putStrLn ("keep " ++ (show x) ++ "?")
  (c : _) <- getLine
  return (c == 'y')

askWhichToKeep :: [Int] -> IO [Int]
askWhichToKeep xs =
  filterM askToKeep xs

-- Monadic List Functions: foldM
-- The fold combines all the values in the list together into a single value.
-- foldM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a -- the first argument to foldM is the function used to combine the values. Which takes an accumulator `a` and a value from a list `b` retuns a monadic action that produces the next accumulator value. The second argumetn to foldM is the inital accumulator value and the third argument is the list `b` of values to be combined. It returns a monadic action that produces the final accumulator value after all the combining operations
-- foldM allows the combining operation in the fold to be a monadic computation.


sayAddition :: Int -> Int -> IO Int
sayAddition x y = do
  let z = x + y
  putStrLn ((show x) ++ " + " ++
            (show y) ++ " = " ++
            (show z))
  return z

talkingSum :: [Int] -> IO Int
talkingSum xs = foldM sayAddition 0 xs

main = do
  printElem
  case justFive of
    Just n -> putStrLn (show n)
    Nothing -> putStrLn "nothing"
