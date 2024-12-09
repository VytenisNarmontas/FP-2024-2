module Lib1
    ( completions
    ) where

-- | This function returns a list of words
-- to be autocompleted in your program's repl.
completions :: [String]
completions = 
    [ "add car",
      "remove car",
      "list cars",
      "service car",
      "list services",
      "save",
      "load"
    ]