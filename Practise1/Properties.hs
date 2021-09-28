import Test.QuickCheck

myInsert :: Int -> [Int] -> [Int]
myInsert x [] = [x]
myInsert x (y:ys) | x < y = x : y : ys
                  | otherwise = y : (myInsert x ys)

myOrdered :: [Int] -> Bool
myOrdered [] = True
myOrdered [x] = True
myOrdered (x:y:ys) = x <= y && myOrdered (y:ys)

prop_RevUnit :: Int -> Bool
prop_RevUnit x = reverse [x] == [x]

prop_RevApp :: [Int] -> [Int] -> Bool
prop_RevApp xs ys = reverse (xs ++ ys) == reverse ys ++ reverse xs

prop_RevRev :: [Int] -> Bool
prop_RevRev xs = reverse (reverse xs) == xs

prop_RevApp_wrong :: [Int] -> [Int] -> Bool
prop_RevApp_wrong xs ys = reverse (xs ++ ys) == reverse xs ++ reverse ys

prop_MaxLe :: Int -> Int -> Property
prop_MaxLe x y = x <= y ==> max x y == y

prop_Ordered :: Int -> [Int] -> Property
prop_Ordered x xs = myOrdered xs ==> classify (null xs) "trivial" $ myOrdered (myInsert x xs)

prop_DoubleCycle :: [Int] -> Int -> Property
prop_DoubleCycle xs n = not (null xs) && n >= 0 ==>
    take n (cycle xs) == take n (cycle (xs ++ xs))  