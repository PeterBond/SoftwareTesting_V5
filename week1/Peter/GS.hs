module GS where

maxInt :: [Int] -> Int
maxInt []    = error "empty list"
maxInt [h]   = h
maxInt (h:t) = max h (maxInt t)

removeFst :: [Int] -> Int -> [Int]
removeFst []    x = error "empty list"
removeFst [h]   x | h == x    = []
                  | otherwise = [h]
removeFst (h:t) x | h == x    = t
                  | otherwise = h : (removeFst t x)

removeFstJ :: Int -> [Int] -> [Int]
removeFstJ m []     = error "empty list"
removeFstJ m [x]    | x == m = []
                    | otherwise = [x]
removeFstJ m (x:xs) | x == m = xs
                    | otherwise = x : (removeFstJ m xs)

count :: Char -> String -> Int
count c []    = 0
count c [h]   | h == c    = 1
              | otherwise = 0
count c (h:t) | h == c    = 1 + count c t
              | otherwise = count c t
