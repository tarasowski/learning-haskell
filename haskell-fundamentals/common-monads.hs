module CM where

import Prelude
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.ST
import Data.STRef

-- Common Monads
-- Reader let's you write computations that have access to context value like (the name of the logged in user)
-- State to express computations that have change in state, such as a simulation
-- ST allows to write high-performance imperative code without loosing Haskell functional purity

-- Reader Monad
-- Is used to express computations that need access to some context value like the name of the current user.
-- Of course you can pass that value as an argument, but sometimes you'll need that value in many different places
-- the Control.Monad.Reader provides a data type `Reader r a` with two arguments:
-- 1) `r` is the type of the context value which can be read within the `Reader` monad. E.g. if the context was the current user name, so the `r` might be a String
-- 2) `a` is the result of the `Reader` computation. This is the type that is packaged up in the `Reader` monad. So If I was computing an Int depending on the user name `a` would be Int.
-- instance Monad (Reader r) -- Reader is an instance of the Monad type class. So everything you can with other monads, like bind, do-Blocks, you can also do with `Reader`.
-- In order to read the context value you use ask :: Reader r r
-- It's a value in the `Reader` monad, who's resulting value that is a value packaged up in the monad. It's just the context value stored in the reader. That's why the context and result type `Reader r r` are both the same type.
-- If we want to do computations depending on the context value, we need a way to supply that context value runReader :: Reader r a -> r -> a
-- runReader takes a value in a `Reader` monad and a context value for the reader to use and returns a value produces by the `Reader` monad for that context value.

getFirst :: Reader String String
getFirst = do
  name <- ask -- ask function holds the context of the monad
  return (name ++ " woke up")

getSecond :: Reader String String
getSecond = do
  name <- ask
  return (name ++ " wrote some Haskell")

getStory :: Reader String String
getStory = do
  first <- getFirst
  second <- getSecond
  return ("First, " ++ first ++
          ". Second, " ++ second ++ ".")

story = runReader getStory "Dimitri" -- the last argument is the context
-- "First, Dimitri woke up. Second, Dimitri wrote some Haskell."


-- State Monad
-- newtype State s a = State (s -> (a, s)) -- a State type itself is just a type alias for a function, which takes a value for the state `s` and returns a pair containing the result of the computation `a` and the new state value `s`
-- This monad is used to write code that uses state that might change during the computation.
-- Certain problems like simulation are formulated in terms of state.
-- Control.Monad.State provides a data type `data State s a`, which takes two type arguments:
-- 1) `s` which is the type of the state
-- 2) `a` which is the result of the state for computation
-- State is an instance of a Monad type class `instance Monad (State s)`, everything you have learned from other monads, applies to `State` as well.
-- get :: State s s -- reads the current State. It's a State monad who's result is the state itself
-- put :: s -> State s () -- to modify the State you can use put function. Put takes a single argument which is the new state value to be stored and return State monad action which stores the new state. The result type of this action `State s ()` is `Unit` because it's used for side-effects and doesn't return anything meaningful.
-- evalState :: State s a -> s -> a -- to get the value back out we use evalState function
-- The evalState function takes a value in a State monad and an initial state. Returns the value produces by the state for computation with that initial state.

harmonicStep :: State (Double, Double) Double
harmonicStep = do
  (position, velocity) <- get -- state `a` is a pair, so pattern matching can be used to get the position, velocity from the state.
  let acceleration = (-0.01 * position)
      velocity' = velocity + acceleration
      position' = position + velocity'
  put (position', velocity') -- the new position' and velocity' become the new state, which can be used if we ran another iteration of the simulation.
  return position -- we return the original position (wrapped back into a monad).

harmonic :: State (Double, Double) [Double]
harmonic = do
  position <- harmonicStep -- updates the state and gives the position
  laterPositions <- harmonic -- call harmonic recursively to get a list of all later positions. Infinite recursion, but thank's to Haskell lazy evaluation that's no problem.
  return (position : laterPositions)

--          evalState :: State s a -> s -> a
positions = evalState harmonic (1,0)
eight = take 8 positions

-- ST Monad
-- Is a a souped-up "aufgemotzt" version of the State Monad.
-- It allows you to implement imperative algorithms
-- When the State monad gives you one updatable value representing the state. The ST Monad gives you access to unlimited number of values, that can be each updated independenlty of the other. Like you would have in an imperative language.
-- data ST s a. Just for the Reader and State monad the second type argument `a` corresponds to the result type of the computation.
-- However the first type variable `s` plays a more complicated role. In order to use ST monad you may ignore `s` entirely. It's not a type that you choose and won't have any meaningful values.
-- `s` is used by the type system to enfore functions purity by preventing unrelated ST computations from sharing values.
-- instance Monad (ST s) -- ST is an instance of the Monad type class and gets all the beneifts that come alongs with that.
-- runST :: ST s a -> a -- this functions turns an ST value into an ordinary value.

-- Now you have ST Monad you need to declare updatable variables. These comes from different module `Data.STRef`
-- This module defines a data type `data STRef s a` which represents a reference to a value of type `a` or in other words updatable value of type `a`
-- The `s` type variable here is exactly the same as `s` type variable as on the ST MOnad type. It keeps the computation which ST this `s` belongs to. And won't let you use it in another ST computation.
-- newSTRef :: a -> ST s (STRef s a)
-- It takes a single argument which is the initial value with which STRef should start off. It returns ST computation that's result is the newly created STRef
-- In order to read the value of STRef you you readSTRef :: STRef s a -> ST s a -- it takes STRef to read from and returns ST value whose result is in that STRef.
-- writeSTRef :: STRef s a -> a -> ST s () -- the values can be updated with the writeSTRef function. This function takes STRef s a and new value to store `a` and produces ST s () action that will perform that update.
-- ST Monad allows you to write faster code. It can help you to translate an alogrithm from a high-performance language into Haskell. But you should better change your code to functional style. Complicated, multi-part state. It shouldn't be the main goal to use it in Haskell...
main = putStrLn "Hello World"
