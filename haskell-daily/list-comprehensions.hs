
--Source: https://wiki.haskell.org/List_comprehension
import Data.Char (toUpper)

-- List comprehensions in Haskell
listT2 = [x^2 | x <- [1..5]]
-- [1,4,9,16,25]
-- The symbol | is read as `such that``
-- <- is read as is drawn from
-- x <- [1..5] is called a generator

listP = [(x,y) | x <- [1,2,3], y <- [4,5]]
-- [(1,4),(1,5),(2,4),(2,5),(3,4),(3,5)]

toU s = [toUpper c | c <- s]
-- "HELLO WORLD"
-- where s :: String such as "Hello World". Strings in Haskell are lists of characters. The generator `c <- a`feeds each character of `s` in turn to the left-hand expression `toUpper c`, building a new list. 

toUMap s = map toUpper s
-- "HELLO WORLD"

main = print (toUMap "Hello World")
