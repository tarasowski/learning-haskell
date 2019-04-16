module Types where

import Prelude

-- Types Systems
-- Static Types                                                                         vs. Dynamic Types
-- Variables have a fixed type and can hold only values related to that type            Variables can store any type (var x = "hello")
-- int x = "hello" will not compile                                                     There is no type checking and no compiler
-- More errors caught at compile-time int x = "hello" + 3                               More run-time errors (var x = "hello" + 3")
-- Write lots of types (bad)                                                            Never write types (good)
-- Code reuse becomes harder (you need to define statically the type)                   Better code reuse
-- Repeated code for different types                                                    Same code works for many types


-- Haskell's Type System
-- It's very very statically type
--  It can catch lots of compile-time errors
--  Few run-time errors (once it has compiled)
-- Type are inferred
--  The compiler figures it out what type must be
--  Don't hae to write out explicit types
--  Explicit types communicate with PEOPLE (in Haskell), checked by compiler
--  You use types to communicate with other PEOPLE if you use them at all
-- Same code can work for many different types
--  Using polymorphic functions you can use a single function that can handle many different types

main =
  putStrLn (show "Hello World!")
