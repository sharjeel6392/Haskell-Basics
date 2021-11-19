catAll :: [[a]] -> [a]
catAll [] = []
catAll ([]:xs) = catAll xs
catAll ((x:xs):ys) = x:catAll(xs:ys)