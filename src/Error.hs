module Error (report) where

import Text.Printf

report :: Int -> String -> String -> String
report = printf "[line %d] Error%s: %s"
