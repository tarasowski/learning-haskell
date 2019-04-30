module Collections where

import qualified Data.Set as Set
-- Standard collection types in Haskell
-- Set: unordered set of values
-- Map: key / value pair data structure
-- Seq: stores elements in the order, just like List however Seq is more effecient and supports more operations

-- Functional Collections
-- data List a = Empty | Cons a (List a)
-- Immutable - you can never insert an element or change the value of an a list, but instead you can use pattern matching to access elements of the list or create new elements of the list.
-- Function that updates the first element in the list, but what it's actually does it creates a new copy of the list with a new first element
-- updateFirst :: List a -> a -> List a
-- updateFirst Empty y = Empty
-- updateFirst (Cons x xs) y = Cons y xs
-- Because values never change it's possible to share immutable data
-- Immutable modification of an collection can be very slow, but Haskell uses a referce to original collection (which is fast)
-- Queries like what is the 10th element of an array, are pure and don't cause performance issues


-- Set
-- Is an unordered collection of values. 
-- It means that this data structure can only tell you whenever a particular value is in a Set or not. But it can't tell you what order the elements where inserted into the set like a List can.
-- If you use a set with other datat type, you may want to use a qualified import

-- empty :: Set a
-- insert :: Ord a => a -> Set a -> Set a
-- delete :: Ord a => a -> Set a -> Set a
-- union take two Sets of type and returns a new Set consisting elements that is either one of those two Sets
-- union :: Ord a => Set a -> Set a -> Set a
-- member functions allows us to query the set
-- member :: Ord a => a -> Set a -> Bool

newS = Set.insert 5 Set.empty
newS' = Set.insert 10 Set.empty
deleteS = Set.delete 5 (newS)
unionS = Set.union newS newS'

-- Set Restrictions
triple :: Int -> Int
triple x = x + x + x

triple' :: Int -> Int
triple' x = 3 * x

funSet :: Set (Int -> Int)
funSet = Set.insert triple S.empty

-- It's not going to work, cause functions doesn't have the equality operation and are not instances of the Eq type class
-- Sets only work with types in the class Eq a 
-- For performance reasons they need to be in a more tightly/restricted class Ord a. Ord is a type class for types that have some notion of ordering like Ints with usual ordering 0,1,2,3,4, Strings with alphabetical ordering a,b,c,d,e, or almost any other type you can define except things like functions.
problem :: Bool
problem = member triple' funSet



main :: IO ()
main = putStrLn "Hello World"
