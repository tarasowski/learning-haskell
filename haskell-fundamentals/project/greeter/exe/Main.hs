module Main where

import System.Environment
import Greeter

main = mapM_ (putStrLn . greet) =<< getArgs
