comb :: [a] -> [(a, [a])]
comb [] = []
comb (x:xs) = (x,xs) : [(y,x:ys) | (y,ys) <- comb xs]

perm :: [a] -> [[a]]
perm [] = [[]]
perm xs = [ y : ps | (y,ys) <- comb xs, ps <- perm ys]
