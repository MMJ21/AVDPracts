module Increment where
increment :: Int ->Int
increment x = x + 1

equals :: Int -> Int -> Bool
equals x y = if x == y then True else False

equalToZero :: Int -> Bool
equalToZero 0 = True
equalToZero _ = False

equalToZeroTwo :: Int -> Bool
equalToZeroTwo x | x == 0 = True
                 | otherwise = False

findInList :: [Int] -> Int -> Int
findInList (x:xs) y | (length (x:xs) == y) = findInList xs (y - 1)
                | (length xs > y) = findInList xs y
                | (y > length (x:xs)) = -1
                | otherwise = x