module Org where

encrypt :: Char -> Char
encrypt c
  | 'A' <= c && c < 'Z' =
    toEnum (fromEnum 'A' + 1)
  | c == 'Z' = 'A'
  | otherwise = c

-- Do as little in IO as possible
-- You just need enough IO to get data to and from your program
{-
 - Poorly Designed Program
 

handleChar :: IO ()
handleChar = do
  c <- getChar
  let u = encrypt c
  putChar c

inputLoop :: IO ()
inputLoop = do
  handleChar
  inputLoop

main :: IO ()
main = inputLoop
-}



-- Well designed program
-- Do as little IO as possible
main :: IO ()
main = interact (map encrypt)
