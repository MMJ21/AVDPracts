prop_RevUnit x = reverse [x] == [x]
prop_RevApp xs ys = reverse (xs ++ ys) == reverse ys ++ reverse xs
prop_RevRev xs = reverse (reverse xs) == xs