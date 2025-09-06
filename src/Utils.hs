module Utils ((!?)) where

(!?) :: [a] -> Int -> Maybe a
xs !? n
  | n < 0 || n >= length xs = Nothing
  | otherwise = Just $ xs !! n
