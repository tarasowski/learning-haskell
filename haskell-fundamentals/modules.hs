module M where

-- Large code organisation is Haskell is done through Cabal packages
-- Cabal packages contain several related modules which can be installed together
-- Packages can depend on other packages


-- Importing Modules
-- Import statements begin with the keyword `import` followed by the name of the module `Data.Set`
-- In this case we are importing `Data.Set` a standard module.
-- import Data.Set -- this is a basic import statement
-- The variable `empty` is now accesible.


-- Import qualified
-- If you want import a module and don't want to create name conflicts
-- `Data.Set` exposes all variables such as `empty :: Set a ` that can be used inside the file
-- Another module can do import the same variables and there will be name conflicts
-- e.g. `Data.Set` and `Data.Sequence` define both a variable name empty. One is an empty set and other is an empty sequence
-- If we import both modules and ask ghci what `:t empty` is, we'll get a message `Ambiguous occurence empty`
-- In many cases you want to refer to imported variables by their fully qualified names
-- import qualified Data.Set
-- import qualified Data.Sequence
-- Now after qualified import, we can get `Data.Set.empty` the variables that we need.
-- But we can also define as shorter name by adding `as` keyword followed by a Name.
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
-- Now we can get the `empty` variable from the module as `Seq.empty`


-- Functions - Explicit Import Lists
-- If you want to have a fine-grained control which variables are imported and which are not
-- You can provide a list of which variables you want to import in parens separed with comma (fn1, fn2)
-- A good practice to use explicit import lists like this:
-- import Data.Set (empty, size)

-- Data Types - Explicit Import Lists
-- You can also import data types.
-- This will import the Maybe data type, from `Data.Maybe` module, but it would not import any of the constructors. 
-- import Data.Maybe (Maybe)
-- If you want to import the constructors you can put them by comma separeted list in the parens
-- import Data.Maybe (Maybe (Just, Nothing))
-- If you want to import all constructors from a data type, you can put (..) two dots in parens
import Data.Maybe (Maybe (..))

-- Type Classes - Explicit Import Lists
-- You can also import type classes in a explicit import list.
-- import Control.Modad (Monad) -- this will import the class by itself but not the functions defined in the type class
-- import Control.Monad (Monad, return) -- to import all functions from a type class you import them just regular functions
-- The line above imports the type class Monad as well as the function return defined in the type class
-- Type class instance ignore any explicit import list
-- Automatically imported with the module (this is the behavior you want to have)
-- If you only interested importing a type class instance in the module you can specify an empty import list, s. below: 
-- import Data.Set ()


-- Import Hiding
-- If you want to import everything from a module but you want to avoid importing some of the names that conflict with something else
-- import Data.Set hiding (empty,size) -- you can list names that should not be imported. Everything will be imported except empty, size functions. 
-- import Prelude hiding (map) -- if you want to hide automatically imported functions such as map that are automatically imported into every file.
-- however you can explicitly import Prelude, adjust the name and hide some of the names
-- import qualified Prelude as P, all the functions become accessible with prefix P. (P.map) (P.filter)


-- Defining modules
-- Defining modules is an easy way to separate your code into units
-- A module is a Haskell source file that start with a line:
-- module MyModule where
-- starts with `module` keyword, then the name and `where` keyword and says, the stuff that follows is defined inside MyModule module.
-- In order for the compiler to find the module, the file name should be the same as the module name: MyModule.hs in the src directory

-- Hierarchical module names
-- The name is separated by dots
-- module Foo.Bar.Baz where
-- File name: Foo/Bar/Baz.sh

-- Export Lists
-- When you define a module you will have an external API and internal helper function that you don't want other to call
-- By default everything exported by default and can be used by any code imported in this module 
-- However you can give an explicit export list, that only the functions that you want are exported 
-- An explicit export list comes between the module name and the where keyword
-- module Foo.Bar.Baz
--  (myFunction
--  , MyType,
--  , MyType2 (Constructor)) where

-- All type class instances (defined or imported) always exported

-- Exporting Parts of Another Module
-- By default imported functions and types aren't exported
-- module Foo.Bar.Baz
--  (fromMyModule) where
-- import MyModule (fromMyModule) -- this line doesn't gets exported

-- Reexporing an entire Module
-- module Foo.Bar.Baz
--  (module myModule) where
-- import MyModule



-- Cabal
-- Haskell has a collection of libraries. These libraries have dependencies. Such as downloading a file is not a practical action.
-- Cabal is Haskells format for Haskell packages. It allows to define my things about the package. Including what other packages it depends on
-- Hackage: online repository of packages
-- cabal-install: command line tool to install packages from Hackage


-- Installing a package


main = putStrLn "Hello World"
