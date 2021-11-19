index :: Eq a => a -> [a] -> Maybe Int
index a xs = index 0 xs
   where 
    index idx [] = Nothing
    index idx (x:xs)   | a == x = Just idx
                       | otherwise = index (idx+1) xs
