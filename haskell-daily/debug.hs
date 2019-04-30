module Main where

import qualified Debug.Trace as Debug

sum' :: Int -> Int -> Int
sum' a b =
  (Debug.traceShowId a) + (Debug.traceShowId b)

result = sum' 2 2

main = do
  putStrLn "Hello World"
  -- Hello World
  putStrLn (show result) 
  -- 2
  -- 2
  -- 4
