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

-- Newtype
-- Defines a type for a type synonym which is not interchangable 
-- If we identify customer by some Int, it would be great if we can identify the customer by Int
-- A new type definition start with a newtype keyword, followed by the name, then = sign and constructor name `MakeCustomerId` and the representation type Int
-- In this case our CustomerId is represented by an Int
newtype CustomerId = MakeCustomerId Int

-- badCustomer :: CustomerId
-- badCustomer = 13


-- when you do this, you are saying turn this Int into CustomerId
customer :: CustomerId
customer = MakeCustomerId 13

-- how to get the CustomerId back? With pattern matching

customerToInt :: CustomerId -> Int
-- pattern matching the constructor. You are giving the constructor backwards. You are giving the CustomerId and you know the only way that CustomerId could have been created is by calling MakeCustomerId constructor with an Int as the argument.
customerToInt (MakeCustomerId i) = i

-- RECORDS
-- Records in Haskell allow you to define a type with several named fields.
-- A record definition starts with the `data` keyword. After the the of the record `Customer`. After the constructor name `MakeCustomer`. After come the field definitions.
-- Each field definition consists of a name a double column and type
data Customer = MakeCustomer
  { customerId    :: CustomerId
  , name          :: String
  , luckyNumber   :: Int
  }

-- To create a customer record you need a following syntax
alice :: Customer
alice = MakeCustomer
  { customerId = MakeCustomerId 13
  , name = "Alice"
  , luckyNumber = 42
  }
-- How do you access the fields?
-- When you define a record type, some magic happens behind the scenes that defines a function for each field behind the record.
-- to access the customerId field you need just to call the `customerId alice` function and pass the record value `alice` as the argument

-- How to update a record?
-- first you take the record value `alice` and pass the updated values in the curly brackets.
sally = alice {name = "Sally", luckyNumber = 94}

-- Limits of the records:
--  They are not extensible. There is no way to create a hierarchy like in the example below.
--  data Person = Person {name :: String}
--  data Customer = Customer extends
--                    Person { luckyNumber :: Int }
-- No shared field names.
-- In both examples the Customer and Supplier have a field `name` this will not work.
--  data Customer = Customer
--    { name      :: String
--    , custmerId :: CustomerId
--    }
--  data Supplier = Supplier
--    { name        :: String
--    , supplierId  :: SupplierId
--    }
-- Due to these limits, the records should be avoided for the aggregate data in Haskell. Instead you should always use an Algebraic Data Type.

-- Algebraic Data Types
-- Almost every type you use in Haskell will be an algebraic data type
-- A customer implemented as an ADT
-- ADT definition starts with a `data` keyword
-- followed by a name of the type
-- followed by a constructor name `MakeCustomer` (Haskell constructors have no code, it's just a value that glues some other values together.
-- after the constructor name comes the type of the arguments to the constructor
-- in this case it will be CustomerId (customer id of the created customer), String (the name of the customer), Int (the lucky number of the customer)
-- unlike in Records these values are not identified by their field name, they are identified by their position.
data CustomerADT = CustomerADT CustomerId String Int
-- the name of the ADT and the constructor names can be the same. This is very common.

dimitri :: CustomerADT
dimitri = CustomerADT (MakeCustomerId 13) "Dimitri" 11

-- to extract the value from an ADT you use pattern matching
getCustomerId (CustomerADT (MakeCustomerId cust_id) name number) = cust_id

-- when you do large pattern matching. It can become cumbersome to give names to arguments of the constructor. So Haskell provides a shortcut, if you put underscore in a pattern it's a wildcard. Any underscore will match anything but won't save any matching variable to a value.

getCustomerIdWild (CustomerADT cust_id _ _) = cust_id

getCustomerName (CustomerADT (MakeCustomerId cust_id) name _ ) = name

-- ADT are closely related to some other data types we have already seen.
-- ADT looks like a newtype definition. The major difference is that ADT support one than more argument.
-- x :: (Double, Double, Double) - a tuple example
-- an example with ADT. It makes more clear of what type x is about.
-- data RGB = RGB Double Double Double
-- x :: RGB

data StringTree = StringTree String [StringTree]
hierarchy = StringTree "C:"
              [ StringTree "Program Files" []
              , StringTree "User"
                [StringTree "Alice" []]
              , StringTree "Cats" []
              ]

-- ALGEBRAIC DATA TYPES
--  Package some values together
--  Into a named container
--  ADT are containers for some values. 

-- ADT CONSTRUCTORS
-- ADTs can have multiple constructors. Which is unique to Haskell.
-- Here is data type Bool' with two constructors False and True
-- False and True are separated by a pipe character
-- Neither these constructors take any parameters. Because they don't need any.
-- There are two ways to construct a Bool' value. False and True
data Bool' = False | True

-- Calling False | True gives a Bool' value
x :: Bool'
x = False
y :: Bool'
y = True

-- Bool values can be used in pattern matching. This is the negate function. Which negates a boolean value.
negate :: Bool' -> Bool'
negate True = False
negate False = True

-- Another example for a ADT
-- Like Bool' it has several constructors without any arguments
-- Types like these are called enums
data DialogResponse = YES | NO | Help | Quit

-- This example can come from a reading from a config file. Where NoInt specifies no value and JustInt specifies a value.
-- This is related to the notion of nullable value found in some other languages.
-- Maybe types in Haskell are very valuable because it has no built in notion of null.
-- In other languages like Java null values are used or abused. In Haskell with Maybe you can spicify the error case that is more safe without null checks etc.
data MaybeInt = NoInt | JustInt Int

-- pattern matching with ADT type constructors
defaultInt :: Int -> MaybeInt -> Int
-- the first definition has pattern that matches when a MaybeInt value was created using the NoInt constructor. Meaning it doesn't contain any value. In this case the default value is returned.
defaultInt defaultValue NoInt = defaultValue
-- this pattern matches when the MaybeInt was created using the JustInt constructor with x as the parameter. In this case `x` is the value to return.
-- we are using underscore here, because if JustInt has a value, we can simply ignore the defaultValue
defaultInt _ (JustInt x) = x

-- the first constructor represents an empty list
-- the second constructor represents a non-empty list with a head and a list of StringList
data StringList = EmptyStringList
                | ConsStringList String StringList

-- again we can use pattern matching
lengthStringList :: StringList -> Int
lengthStringList EmptyStringList = 0
lengthStringList (ConsStringList _ xs) =
  1 + lengthStringList xs


-- ADT TYPE CONSTRUCTORS
--  The big idea of them is that they correspond to different types of values that a type can have.
--  This kinds of values can be very simple with just a single possible value represented by a constructor with no arguments
--  or very complex with many arguments

-- PARAMETRIZED TYPES
-- Parametrized types can hold values of any type!!! 
-- Some types like lists have a slot that can be filled with any type. For example you can have a list of String, Int, Double, Float etc.
-- You can create your own types like this using ADT with type parameters.
-- It's closelsy related to polymorphic functions or generic types from other OOP languages.
-- This generic Maybe type represents a value that has a type variable `a` following the type name `Maybe`
-- Types with type variable like this are called parameterized types
-- This indicates that Maybe' is not itself a type, but if you supply some type of `a` it can produce a type. 
-- The defintion of Maybe is just like ordinary ADT. Except that a type variable `a` can appear in the constructor arguments.
data Maybe' a = Just a | Nothing

-- x can have a Maybe of Int

x' :: Maybe' Int
x = Nothing

-- we can define a polymorphic function
fromMaybe :: a -> Maybe a -> a
fromMaybe defaultVal Nothing = defaultVal
fromMaybe _ (Just x) = x


main = do
  putStrLn "Hello World!"
  putStrLn (show (customerToInt customer))
  putStrLn (show (customerToInt (customerId alice)))
  putStrLn (show (name alice))
  putStrLn (show (name sally))
  putStrLn (show (getCustomerName dimitri))
  putStrLn (show (getCustomerId dimitri))
