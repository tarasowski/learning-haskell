module CustomTypes where

-- Type Synonyms
-- The purpose of Type Synonyms is to make code more readable
-- Helps you understand the semantic meaning
-- Can define a new name which can be used for existing types
-- A type synonym defintion start with a `type` keyword and Uppercase letter name for a type synonym
type String' = [Char]

-- This type synonym defines a type synonym Point for a pair of (Double, Double)
type Point = (Double, Double)

-- (Double, Double) -> (Double, Double) -> (Double, Double)
midpoint :: Point -> Point -> Point
midpoint (x1, y1) (x2,y2) =
  ((x1 + x2) / 2, (y1 + y2) / 2)

-- Type synonyms are interchangable with the type declaration. You can mix and match them.
-- Type Synonyms are completely ignored by compiler. If you want type Point and the pair of (Double, Double) not to be interchangable. You need to define a New Type.

p1 :: (Double, Double)
p1 = (1,2)

p2 :: Point
p2 = (3,4)

mid :: (Double, Double)
mid = midpoint p1 p2

main = 
  putStrLn "Hello World!"
