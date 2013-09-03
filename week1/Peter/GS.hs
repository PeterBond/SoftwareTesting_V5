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

blowup :: String -> String
blowup s = let 
             blowHelper :: Int -> String -> String
             blowHelper n [] = []
             blowHelper n [h] = replicate n h
             blowHelper n (h:t) = replicate n h ++ blowHelper (n+1) t
           in
             blowHelper 1 s

srtString :: [String] -> [String]
srtString []  = error "empty list"
srtString [h] = [h]
srtString s   = let
                srtHelper :: [String] -> String
                srtHelper [h]   = h
                srtHelper (h:t) = max h (srtHelper t)
              in
                srtHelper s : srtString (drop 1 s)

prefix :: String -> String -> Bool
prefix [] ys = True
prefix (x:xs) [] = False
prefix (x:xs) (y:ys) = (x==y) && prefix xs ys

substring :: String -> String -> Bool
substring (x:xs) (y:ys) = prefix xs ys
substring str1 (y:ys)   = prefix str1 (y:ys) || substring str1 ys

lengths :: [[a]] -> [Int]
lengths a = map length a

sumLengths :: [[a]] -> Int
sumLengths a = sum (lengths a)
