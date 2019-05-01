{- # LANGUAGE ViewPatterns #-}
module Collections where

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
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

-- funSet :: Set.Set (Int -> Int)
-- funSet = Set.insert triple Set.empty

-- It's not going to work, cause functions doesn't have the equality operation and are not instances of the Eq type class
-- Sets only work with types in the class Eq a 
-- For performance reasons they need to be in a more tightly/restricted class Ord a. Ord is a type class for types that have some notion of ordering like Ints with usual ordering 0,1,2,3,4, Strings with alphabetical ordering a,b,c,d,e, or almost any other type you can define except things like functions.
-- problem :: Bool
-- problem = Set.member triple' funSet

-- Map
-- Key-value pair collection
-- It allows you to find a value associated with a key
-- data Map k a
-- empty :: Map k a
-- insert :: Ord k => k -> a -> Map k a -> Map k a
-- If the old Map contains the same key as we are trying to insert. The old value get's overriden.
-- delete :: Ord k => k -> Map k a -> Map k a
-- union :: Ord k => Map k a -> Map k a -> Map k a
-- If the Maps have the same keys, the value from the left Map is used as a resulting Map
-- If you want more control about the collisions, you can look up at Hackage documentation,there are more functions.
-- lookup :: Ord k => k -> Map k a -> Maybe a

dict = Map.empty
user1 = Map.insert "user1" "Dimitri Tarasowski" dict
user2 = Map.insert "user2" "Anastasia Mudrova" dict
user3 = Map.insert "user3" "Elon Musk" dict
together = Map.union user1 user2
withElon = Map.union together user3
-- fromList [("user1","Dimitri Tarasowski"),("user2","Anastasia Mudrova"),("user3","Elon Musk")]
isDimitri = Map.lookup "user1" withElon

-- Seq
-- Is an ordered collection like a list. Seq can do everything like a list can do: appending a new element to the front or pattern matching to split of the first element.
-- However Seq supports many other operations that are super slow with a traditional lists
-- The module is named Sequence, and a data type is called Seq
-- empty :: Seq a
-- (<|) :: a -> Seq a -> Seq a -- to add an element to a Seq. It's like cons (:) operator on the Lists
-- (|>) :: Seq a -> a -> Seq a -- to add an element to the back of the Seq. It's a very efficient operation in comparison to Lists
-- (><) :: Seq a -> Seq a -> Seq a -- concat to Seq's together. Concatination of lists is much much slower.

-- Seq Pattern Matching
-- In order to pattern match on Seq we need to turn it into a View Patterns
-- View Patterns is a language extention that must be turn on by putting a comment on top of the source file. You want to use view patterns!
-- {- # LANGUAGE ViewPatterns #-}
-- length function transforms a Seq into an algebraic data type called viewl :: Seq a -> ViewL a
-- length :: Seq a -> Int
-- length (viewl -> EmptyL) = 0
-- length (viewl -> x :< xs) = 1 + lenth xs
-- length' :: Seq a -> Int
-- length' (viewr -> EmptyR) = 0
-- length' (viewr -> xs :> x) = 1 + length xs
-- Seq usually faster than list. But if you're dealing with short lists and want to look at the head. List are fast too.
-- Almost all operations are much faster on Seq's
-- However they do come with some complexity like View Patterns

main :: IO ()
main = do
  putStrLn "Hello World"
  case isDimitri of
    Nothing -> putStrLn "Not Dimitri"
    Just name -> putStrLn name

