{-
 - What is Haskell?
  - Haskell is purely functional, lazy sctically typed programming language
    - Purely functional
      - Function are first class values (can be manipulated and passed around like an Integer)
      - Values never change - instead new copies are created
    - Lazy (non strict)
      - The code is never executed until the value it computes actually needed
      - If I have a variable in my programm, that requires computation but I have never actually used it. It will never get computed. 
      - If you have a huge data structure and your program only use small part of it. The most data structure will never be created.
    - Statically Typed
      - It means you never use a string if you meant to use an Integer
      - Static types has a drawback: 
        - that's why dynamic types languages were created. But Haskell looks and feels like a dynamic type but has all the tools and safety from statically typed language.
-}

{-
 - Why Shoud I Learn Haskell?
 - C-Engineering is designed by Engineers to work closely on the bare bode metal
 - Java/C# - Business, not about performance but more about the business needs
 - Haskell - Mathematics. Comes from a Math background and is a product of math theories implemented by academic research.
  - Patient development by phd guys (not with focus on users, investoers etc.)
  - Strong theoretical roots from math. (in the begining no I/O possible, then someone came with Monads)
  - Fosters innovation
- Strong performance (10x times faster than Python?)

-}

string1 = "hello"
string2 = "world"
greeting = string1 ++ " " ++  string2

