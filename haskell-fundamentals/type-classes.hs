module TC where

-- Type Classes
-- 
-- elem function tests whether the value appears in the list
elem' _ [] = False -- the first definition embends the case when the list we are searching for is empty
elem' x (y : ys) -- the second defintion uses pattern matching that handles the case when the list is non-empty. The pattern matching part here for the list (y : ys) means take the head `y` and the tail `ys`
  | x == y    = True -- this guard  tests whether `x` is equal to `y`
  -- IMPORTANT: the import thing to notice here is that we use == to test wheter x and y are equal. Some languages have equality test built-in but not Haskell. Some types like functions cannot be tested for equality at all. 
  | otherwise = elem' x ys -- this guarded expression handles everything that the first case rejects

-- In order for elem function to work with any type that has equality test.
-- The type of the elem' function uses the type class constraint Eq a. Many standard types like Int, String are automatically in the Eq class type
elem' :: Eq a => a -> [a] -> Bool

-- What if we want to use a custom type?
data RGB = RGB Int Int Int
colors = [RGB 255 0 0, RGB 0 255 0, RGB 0 0 255]
green = RGB 0 255 0
greenInColors = elem' green colors -- this won't work because RGB is not in the Eq type class
-- the solution is to create an instance of the Eq type class for the RGB type
-- this is how you tell the compiler that RGB types can be compared for equality

-- A type class instance starts with the instance keyword
-- then the name of the type class we are defining the instance of `Eq`
-- after that is the type we are defining instance for `RGB`
instance Eq RGB where
-- this definition uses pattern matchng and says one RGB value is equal to another RGB value.
-- `==` is just a function. So we need to implement this function that the Eq type class requires. `==` expectes two arguments for equality
-- (RGB r1 g1 b1) == (RGB r2 g2 b2) is the same as below
  (==) (RGB r1 g1 b1) (RGB r2 g2 b2) =
  -- when the r1 == r2 && g1 == g2 && b1 == b2 are equal
    (r1 == r2) && (g1 == g2) && (b1 == b2)

-- Lets say we want to represent our RGB values as a string
-- The show function is the usual way to do this
-- If you try to call show on RGB you will get an error no intance for show on RGB. This is because show uses type class and only certain values can be shown.
-- Values like functions have no meaningful value representation and cannot be shown. So we need an instance for the show type class for RGB

instance Show RGB where
  -- here we implement any functions that a show type class requires. Here is just the show function.
  -- we use pattern matching on the RGB value we need to turn into a string. This extract the components to the `r` `g` `b` variables.
  show (RGB r g b) =
  -- the string is formed by appending strings togehter (show r) converts the r into a string. In this case r is Int
    "RGB " ++ (show r) ++ " " ++
    (show g) ++ " " ++ (show b)

-- Deriving Type Class Instances
-- Most of the types you create have obvious way to implement equality comparisons. But it would be very hard to do it all the time.
-- So Haskell provides a way to generate them automatically. To do this you need to add the `deriving` keyword to the ADT and then the type class `Eq`
-- This says give RGB the Eq operator. 

data RGB' = RGB' Int Int Int
  deriving (Eq, Show)

-- The compiler isn't magic and can't guess any instance for a possible type class
-- Deriving works only with a few standard classes
--  Eq 
--  Ord - (<), (>) etc.
--  Show - show (parses a value into a string)
--  Read - read (parses a string into a value)

blue = RGB' 250 250 250
colors' = [RGB' 250 0 0, RGB' 250 250 250]

redInBlue = elem' blue colors'

-- A function type
-- data Foo = Foo (Int -> Int) ----- but you can't derive anything here because there is no Eq type class for functions.

-- Defining Type Classes
-- Many languages have backed up the Eq class but not in Haskell
-- Haskell has a Eq class that is defined in the following way
-- It starts with the a `class` keyword then there is a name of the class being defined `Eq` followed by a type variable `a`
-- `a` stands for a type in the Eq class, then there is the `where` keyword. The functions that Eq requires are (==) (/=)
-- class Eq a where
--  (==) :: a -> a -> Bool
--  (/=) :: a -> a -> Bool
-- The minimum set of functions that an instance have to implement is the `minimum complete definition` for Eq this is: (==) or (/=)
-- In addition you may define your own type classes.
-- Overloading a function that should operate on different types, but with different behavior for each type. You should create a type class.

data Point2 = Point2 Double Double
  deriving Show
data Point3 = Point3 Double Double Double
  deriving Show

distance2 :: Point2 -> Point2 -> Double
distance2 (Point2 x1 y1) (Point2 x2 y2) =
  sqrt (dx * dx + dy * dy)
  where dx = x1 - x2
        dy = y1 - y2

pathLenght2 :: [Point2] -> Double
pathLenght2 [] = 0
pathLenght2 (_ : []) = 0
pathLenght2 (p0 : p1 : ps) =
  distance2 p0 p1 + pathLenght2 (p1 : ps)

-- In order to have more code reuse we can create a new type class
-- The class of types where you can measure the distance between things
class Measurable a where
  distance ::  a -> a -> Double

instance Measurable Point2 where
  distance = distance2

instance Measurable Point3 where
  distance (Point3 x1 y1 z1) (Point3 x2 y2 z2) =
    sqrt (dx * dx + dy + dz * dz)
    where dx = x1 - x2
          dy = y1 - y2
          dz = z1 - z2

-- now function distance works with both data types Point2 and Point3  
-- now we can write a polymorphic pathLength function that can work with any type of type class Measurable
-- Because of the type calss measurable it's going to call the distance function whichever is appropriate for that type
pathLength :: Measurable a => [a] -> Double
pathLength [] = 0
pathLength (_ : []) = 0
pathLength (p0 : p1 : ps) =
  distance p0 p1 + pathLength (p1 : ps)

-- Subclasses of Type Classes
-- Sometimes type classes are logically related to each other. Sometimes is one is necessary for the other
-- Ord (<), (<), (<=), (>=) - Ord type class represents a type that can be compared with less lt, gt, gte, lte.
-- Such types should have an equality test. The `Ord` type class is a subtype of `Eq` meaning every type in the `Ord` type class must also be in the `Eq` type class.
-- We also say `Eq` is a super class of `Ord`
-- This definition below indicates the Ord is a subclass of `Eq`
-- class (Eq a) => Ord a where
--  (<)       ::  a -> a -> Bool
--  (<)       ::  a -> a -> Bool

-- We have implemented Algebraic Data Types. To represent the data for 2 and 3 dimensional space
-- data Point2 = Point2 Double Duble
-- data Point2 = Point3 Double Double Double
-- We made both of these instances of the measurable class. Which also allows us to compute the distance between points.
-- class Measurable a where
--  distance :: a -> a -> Double
-- Let's define a type class direction. Representing all types where you can get direction from one point to another
-- When we want to give directions we need a way to measure the distance `Measurable a` and to show a string `Show a`
-- So we make `Directions` a subclass of both `Measurable` and `Show` 
-- This means any type in the Directions subclass must be also in the `Measurable` and `Show` type classes
-- Because we know that type `a` is both measurable and showable, we can use this to implement a default implementation of getDirections function
-- This means we can use the `show` and `distance` functions.
class (Measurable a, Show a) => Directions a where
  getDirections :: a -> a -> String
  getDirections p1 p2 =
    "Go from " ++ (show p1) ++
    " towards " ++ (show p2) ++
    " and stop after " ++ (show (distance p1 p2))

-- Now let's define an instance of the Directions sub-class
instance Directions Point3 where
  getDirections p1 p2 =
    "Fly from " ++ (show p1) ++
    " towards " ++ (show p2) ++
    " and stop after " ++ (show (distance p1 p2))

-- If we want to use the default implementation of Directions for Point2 whe don't need to define any functions. We can use the default functions in sub-class `Directions`
-- This is what we need to write if we want to make Point2 and instance of the `Directions` sub-class
instance Directions Point2 where

main = do
  putStrLn "Hello World!"
  putStrLn (show green)
  putStrLn (show (RGB 255 0 255))
  putStrLn (show (redInBlue))
