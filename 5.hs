evenSquares' :: [Int] -> [Int]
evenSquares' [] = []
evenSquares' lst = map (^2) (filter even lst)