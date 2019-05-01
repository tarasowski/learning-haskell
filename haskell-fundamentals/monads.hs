module MO where


-- Monads 
-- Monads allow you to generalize your code by using a common pattern
-- do-Notation makes Haskell code more readable and intuitive

-- Monad Examples - IO
-- The most important example of a Monad in Haskell is the IO type
-- Remember: An IO value is a value that may depend or influence the outside world (through user, disk, internet, or any other communication outside the program itself)
-- Haskell unlike most languages, distinguishes these value with side-effects like a number that an user typed in from pure values like 7*3
-- Any pure value can be turned into an IO value, who's side-effect happens to do nothing
-- return :: a -> IO a
-- The return function is very important for combining together IO actions.
-- unreturn :: IO a -> a -- there is no such operation. It's not possible to turn IO value into a pure value. If it would be possible the entire notion of a pure function would break down. Because at any point you just can do some IO and turn in back into a pure value.
-- IO is a sort of a Box that can hold a value with a special property, that is easy to put something into the box, but impossible to take it back out.
-- However that wouldn't make a lot of sense when things that go into the Box become totally unusable. 
-- There is a function that the author calls bindIO :: IO a -> (a -> IO b) -> IO b -- that can let you manipulate the values inside the Box, but you have to leave the result in the Box.
-- The bindIO takes IO of type a and a function that describes how that value should be changed.
-- One way to think about this, is that bindIO temporarily unpacks the value from IO, turns it into a temporarely pure value, but the only thing it's allowed to do with this `a´ pure value is to pass it into the (a -> IO b) function. Which packs `a` back into IO b.
-- In summary you can put values into IO, you can manipulate them, as long as you leave them inside IO.

-- Monad Examples - List
-- Just like for IO it's easy to take an ordinary value and turn it into a list. By creating a list with 1 element.
-- singleton :: a -> [a]
-- unsingleton :: [a] -> a -- there is no such a function!!! On the other hand if I give you list and ask for a value back, it's not so obvious what to do. What if there are lots of values in the List? Maybe you take the first element? But then what if the List is empty? Just for IO we can't unpack a list of values into a value. But what to do instead?
-- We can do something to each element in the list and make a list to the result
-- flatMap :: [a] -> (a -> [b]) -> [b]
-- In particular the flatMap function takes a list of values of type `a` and a function which takes a single value of type `a` and returns a list of values of type `b` and flatMap function returns a list of values of type `b`. Which is formed by concatenating all the lists produced by applying a function to each element of list `a` 

-- flatMap [1,7,11] (\x -> [x, x+1])
-- [1,2,7,8,11,12]
-- Similar to bindIO flatMap let's us unpack a value from a list and do something with it. But in the result or results back into a list and giving us the list concatenating all the results.


-- Monad Examples - Maybe
-- data Maybe a = Nothing | Just a
-- Maybe is an algebraic data type which represents something that might contain a value of type a or might be Nothing.
-- Like IO and Lists it's easy to turn an ordinary value into a Maybe. With a `Just` constructor for the Maybe type.
-- Just :: a -> Maybe a
-- What if you want to get a Maybe value and get an ordinary value back out
-- unJust :: Maybe a -> a -- just like in the previous two cases this doesn't work, because their might not be any value there if the Maybe value was nothing.
-- Maybe has also an analog of bindIO and flatMap, which we'll call bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
-- bindMaybe takes a Maybe value of type `a` and a function which turns a value of type `a` to a `Maybe b` and bindMaybe returns a `Maybe b`.
-- If we call bindMaybe with `Nothing` as the first argument, then it returns `Nothing` without even looking at the function as the second argument.
-- If the passed value contains a value (Just 0), but the function returns `Nothing` then bindMaybe again returns `Nothing`
-- bindMaybe (Just 0) (\x ->
--  if (x == 0)
--  then Nothing
--  else Just (2*x)
-- Result: 1
-- bindMaybe (Just 1) (\x ->
--  if (x == 0)
--  then Nothing
--  else Just (2*x)
-- Result: 2

-- Those 3 functions take an ordinary value and package them up
-- return     ::  a -> IO a
-- singleton  ::  a -> [a]
-- just       ::  a -> Maybe a

-- Those 3 functions allow to modify the packaged up value, provided you package it back up before you're finished.
-- bindIO     ::  IO a    -> (a -> IO b)  -> IO b
-- flatMap    ::  [a]     -> (a -> [b])   -> [b]
-- bindMaybe  ::  Maybe a -> (a -> Maybe b) -> Maybe b

-- Any type with these two operations: 1) packaging up, 2) modify something inside the package IS A MONAD!!!



-- Common Functionality for All Monads
-- The three packaging functions from above, really should be the same function, let's call them return
-- Any monad must support return function like this. It takes an ordinary value and packages it back up inside the Monad type.
-- return :: a -> IO a
-- return :: a -> [a]
-- return :: a -> Maybe a
-- Also all functions for modifying values inside a Monad are so similar so we can just call all of them bind
-- Every Monad must support a bind operation to modify the value or values inside the Monad
-- bind :: IO a       -> (a -> IO b)      -> IO b
-- bind :: [a]        -> (a -> [b])       -> [b]
-- bind :: Maybe a    -> (a -> Maybe b)   -> Maybe b
-- There are other function that work for any Monad.
-- The join function takes a value that is nested inside two layers of a Monad. Join removes the outer layer.
-- join :: IO (IO a) -> IO a
-- join :: [[a]] -> [a]
-- join :: Maybe (Maybe a) -> Maybe a
-- join mmx = bind mmx id
-- join (Just (Just 7))
-- Just 7
-- join (Just Nothing)
-- Nothing
-- join [[1,2,3], [4,5,6]]
-- [1,2,3,4,5,6] -- join concatenates all the lists together

-- These 3 function for IO, List, Maybe seem rather different at first glance. But the Monad abstraction helps to see that they are, in fact, the same.
-- Because bind should work for all Monads. These 3 notions of join all have exactly the same defintion using `bind`. 


-- Monad Type Class
-- F# hast Lists an Maybe which are Monads. But unlike other languages the type system in Haskell allows code that can be used for any Monad. In other languages you need to define a function for each Monad.
-- One of the most powerful features in Haskell for code reuse are type classes.
-- The type `m` is a Monad, following functions are defined for `m`
-- First a Monad must have a return function, which takes a value and wraps it up inside a Monad type `m`
-- If you think about `m` as IO for example, that's exactly the same return function from above
-- A monad also needs to support a bind operation `(>>=)` bind is an operator, rather than a named function, because this is nicer for chaining together multiple binds. Other than the name this is exactly the bind function from above. 
-- By providing `return` and `bind` functions. This class caputures the common pattern in IO, List, Maybe and all other Monads.
-- class Monad m where
--  return  :: a -> m a
--  (>>=)   :: m a -> (a -> m b) -> m b
-- Common patern that we saw in Lists, IO, and Maybe, allows us to write reusable code that will work for any Monad. 
-- By using a type class constraint we can write function in general way, so that they work for any Monad.
-- join :: Monad m => m (m a) -> m a
-- join mmx = mmx >>= id


-- Important: The monad type class, unlike other type classes like num, is a type class of parameterized types. The type variable `m`is not an ordinary type like Int or String, but it is a type like List, IO, or Maybe which themselves have a type parameter. This is why in the definiton of return, we are able to apply the type variable `m`to the type variable `a`.


-- Monad laws
-- If you are writing your own monad from scratch. You should be awaire couple of equations that the `return` and `bind` functions need to satisfy in order to get a valid Monad.


-- do-Notation
-- A function that should work for all monads can only be written by using the return and the bind operator.
-- This function for example takes two Int values, each inside the same monad `m`, and returns the sum.
-- If you think about the `m` as the IO monad for example. Then the two input IO each ask a user for a number, in which case the resulting the IO action would aks a user for two numbers and then return the result inside IO. The way addM works is that the first argument `mx` passed to the bind operator, which essentially extract the value out of the monad and passes it to the function (\x -> my >>= (\y - return (x + y))) as the `x` argument. Then another bind operator is used to extract the second value and pass it to this function (\y -> return (x + y)) as `y`. And finally with both `x` and `y`, available they can be added together as ordinary Ints and the results can be packaged back up into the Monad unsing return.  
-- addM :: Monad m => m Int -> m Int -> m Int
-- addM mx my = 
--  mx >>= (\x -> my >>= (\y -> return (x + y)))

-- Things can can compilicated when you write monadic code in the style above. Therefore Haskell provides some syntatic sugar, called do-Notation.
-- Rather then using the bind operator, we start off with the do keyword, which starts a do-Block.
-- Inside the do-Block you can have a sequence of manadic values `(mx, my)`. A monadic value is a value is the value that inside a Monad.
-- If you use `<-` inside the do-Block the monadic value on the right `mx`,`my` is unpacked and stored on in the variable name on the left. 
-- So the first line in the do-Block exracts `x`from them `mx` monadic value, and the second line extracts `y`from the `my` monadic value, and the final line uses return to package x + y into the monad.
-- This example of addM' is exactly the same as addM from above. do-Notation is a syntactic sugar, it does the same. It's just a convinient way to write code. It's not a way two write imperative code in Haskell.
-- addM' :: Monad m => m Int -> m Int -> m Int
-- addM' mx my = do
--  x <- mx
--  y <- my
--  return (x + y)

people = ["Alice", "Bob", "Eve"]
items = ["car", "puppy"]
missing = do
  --   This line extracts the value from the people list and stores it in the person variable. But there are multiple people, so it will extract Alice, then Bob, then Eve, and for each of these options, it will do the rest of the actions in the do-Block.
  person <- people
  --  This line extracts the item from the items list. Again there are multiple values, so they will each be extracted one after another and the remaining actions will be done for each of them. 
  item <- items
  -- The last line concatentaes some strings together to form the statement who lost what and then packages it back up in the list using `return`
  return (person ++ " lost a " ++ item)



main :: IO ()
main = putStrLn "Hello World"
