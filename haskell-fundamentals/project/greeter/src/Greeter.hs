module Greeter where

import Data.Text.Titlecase
import Hello

greet :: String -> String
greet = titlecase . hello
