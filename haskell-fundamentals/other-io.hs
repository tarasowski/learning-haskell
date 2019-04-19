module OtherIO where


-- Some Useful IO Actions
-- Everything you might want do in traditional programming languages like network communication, interacting with the operating system even mutable variables can be done with IO


reverseLines :: String -> String
reverseLines input =
  unlines (map reverse (lines input))



main = do
  -- puStrLn :: String -> IO ()
  putStrLn "Hello World!" -- Print a string to the console, and append a new line
  -- getLine :: IO String
  line <- getLine -- Reads a line from the console
  putStrLn line 
  -- print :: (Show a) => a -> IO ()
  print "Hello World2" -- Print string representation of a value
  -- readFile :: FilePath -> IO String
  content <- readFile "./test.txt" -- Reads an entire file as a (lazy) string. If you call readFile an a huge file and use only the beginning of the file, the rest will not be read. On the other hand when you loop over entire file, chunks of the file will be loaded as necessary and the parts you are done with will be garbage collected so the whole file doesn't have to fit in memory. 
  print content
  lineToSave <- getLine
  -- writeFile :: Filepath -> String -> IO ()
  -- takes a filepath and a string and returns an IO action which writes the string to the file
  writeFile "./from-console.txt" lineToSave
  -- appendFile :: FilePath -> String -> IO ()
  -- type FilePath = String  -- It's a type synonym for String
  appendFile "./from-console.txt" "This should be appended"
  -- interact :: (String -> String) -> IO ()
  interact reverseLines
