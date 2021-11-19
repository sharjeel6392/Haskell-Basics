singleton :: [a] -> Bool
singleton [] = False 
singleton [x] = True 
singleton (x:xs) = False