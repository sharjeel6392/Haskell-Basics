-- Problem 1--
second :: [a] -> a
second (x:y:xs) = y
{- 
    Test expressions:
        1. second [1,2,3] = second (1:2:3) = 2
        2. second [4,5,6,7,8] = second(4:5:[6,7,8]) = 2
-}

-- Problem 2 --
singleton :: [a] -> Bool
singleton []        = False 
singleton [x]       = True 
singleton (x:xs)    = False
{-
    Test Expressions:
        1. singleton [1,2,3] = singleton(1:[2,3]) = False
        2. singleton [1] = True
-}

-- Problem 3 --
catAll :: [[a]] -> [a]
catAll []           = []
catAll ([]:xs)      = catAll xs
catAll ((x:xs):ys)  = x:catAll(xs:ys)
{-
    Test Expressions:
        1. catAll (["ab","bc"])  = catAll(("a":"b"):["bc"]) = "a":catAll(["b","bc"]) = "ab":catAll(["bc"]) = "abb":catAll(["c"]) = "abbc":catAll([]) = "abbc"
        2. catAll ([[1],[3,4]]) = 1:catAll([[],[3,4]]) = 1:catAll([3,4]) = 1:3:catAll([4]) = 1:3:4:catAll([]) = 1:3:4:[] = [1,3,4]
-}

-- Problem 4 --
index :: Eq a => a -> [a] -> Maybe Int
index a xs = index 0 xs
   where 
    index ind []                    = Nothing
    index ind (x:xs)   | a == x     = Just ind
                       | otherwise  = index (ind+1) xs
{-
    Test Expressions:
        1. index 'x' "wxyz" = index 0 as "wxyz" where index 0 "wxyz" 
        => since "w"/= "x", index (0+1) "xyz" 
        => since "x" == "x", Just 1

        2. index 'a' "wxyz" = index 0 as "wxyz" where index 0 "wxyz" 
        => since "w" /= "a", index (0+1 = 1) "xyz" 
        => since "x" /= "a", index (1+1 = 2) "yz" 
        => since "y" /= "a", index (2+1 = 3) "z" 
        => since "z" /= "a", index (3+1 = 4) [] = Nothing
-}

-- Problem 5 --
evenSquares' :: [Int] -> [Int]
evenSquares' []     = []
evenSquares' lst    = map (^2) (filter even lst)
{-
    Test Expressions:
        1. evenSquares' [1..10] = map(2^) (filter even [1..10]) = map(2^) [2,4,6,8,10] = [2^2,4^2,6^2,8^2,10^2] = [4,16,36,64,100]
-}

-- Problem 6 --
insert :: Ord a => a -> [a] -> [a]
insert x []     = [x]
insert x (y:ys)    | x <= y     = x : y : ys
                   | otherwise  = y : insert x ys

insertionSort :: [Int] -> [Int]
insertionSort [x]       = [x]
insertionSort (x:xs)    = insert x (insertionSort xs)
{-
    Test Expressions:
        1. insertionSort [3,1,2] = insert 3 (insertionSort [1,2]) = insert 3 (insert 1 (insertionSort [2])) = insert 3 (insert 1 [2])
        => since 1 < 2, insert 3 [1,2]
        => since 3 > 1, 1 : insert 3 [2]
        => since 3 > 2, 1 : 2: insert 3 []
        => [1,2,3]
-}

-- Problem 7 --
insertionSortH :: [Int] -> [Int]
insertionSortH = foldr insert []
{-
    Test Expressions:
        1. insertionSortH [3,1,2] = 3 insert ( 1 insert (2 insert []))
        => 3 insert ( 1 insert [2])
        => since, 1 < 2, 3 insert [1,2]
        => since, 3 > 1, 1 : insert 3 [2]
        => since, 3 > 2, 1 : 2 : insert [3]
        => 1 : 2 : 3 = [1,2,3]
-}

-- Problem 8 --
select :: [a] -> [(a, [a])]
select []     = []
select (x:xs) = (x,xs) : [(y,x:ys) | (y,ys) <- select xs]

perm :: [a] -> [[a]]
perm [] = [[]]
perm xs = [ y : ps | (y,ys) <- select xs, ps <- perm ys]
{-
    Test Expressions:
        perm[1,2]
        => First recursive call to perm calls select, which is called 2 times recursively, putting 1 and 2 in the front on each recursion.
        => The second recursive call to perm calls select on the second element, putting 1 and 2 in the "front" on each recursion.
        => perm [1,2] = [1, y],[2, ys]], where (y,ys) = [2,1] => [[1,2],[2,1]]
-}

-- Problem 9 --
-- Implementation of Peano (9.a) --
data Peano = Zero | S Peano deriving Show
{-
    Test Expressions:
        1. Zero => Zero
        2. S ( S ( S (Zero ) ) ) = S ( S ( S Zero ) )
-}
-- Implementation of add (9.b) --
add :: Peano -> Peano -> Peano
add s Zero  = s
add s (S c) = S (add s c)
{-
    Test Expressions:
        1. add s Zero = s Zero
        2. add s c = S (add s c) => (number of S's in c) ( add s Zero) => (number of S's in c) s
-}
-- Implementation of mult (9.c) --
mult :: Peano -> Peano -> Peano
mult s Zero         = Zero
mult (S s) (S c)    = S (add s (mult (S s) c))
{-
    Test Expressions:
        1. mult S (S Zero) Zero = Zero
        2. multiplication as repeated addition.
           mult s c = add s c times. => add ( add ( add (s s))) if number of S's in c = 3
-}
-- Implementation of fact (9.d) --
fact :: Peano -> Peano
fact Zero = S Zero
fact (S c) = mult (S c) (fact c)
{-
    Test Expressions:
        1. fact (S(S(S Zero))) = mult ( (S(S(S(Zero)))) mult ( S(S(Zero)) ( mult (S Zero) (fact Zero)) )
        => mult ( (S(S(S(Zero)))) mult ( S(S(Zero)) ( mult (S Zero) (S Zero)))
        => mult ( (S(S(S(Zero)))) mult ( S(S(Zero)) (S Zero)))
        => mult ( (S(S(S(Zero))))(S(S(Zero)))
        => S ( S ( S ( S ( S (S Zero ) ) ) ) )
-}

-- Grad Problem 1 --
-- Grad Problem (a) --
meaning :: Num t => Peano -> (t -> t) -> p -> t
meaning Zero    = \s z -> 0
meaning (S n)   = \s z -> s (meaning n s z)

-- Grad Problem (b) --
--fromPeano :: Peano -> Int--  
{-
    Test Expressions:
        1.
        2.
-}

-- Grad Problem 2 --
-- (a) equals --
equals :: Eq a=> [a] -> Bool
equals []                   = True
equals [a]                  = True
equals (x:y:xs) | x == y    = equals (y:xs)
                | otherwise = False
{-
    Test Expressions:
        1. equals [1,1,2] => since, 1 == 1, equals([1,2])
        => since 1 /= 2, False
        2. equals [1,1,1] => since, 1 == 1, equals([1,1])
        => since 1 == 1, equals [] = True
-}
-- (b) pair --
pair :: a -> (a -> b) -> (a -> b) -> [b]
pair n f1 f2 = [f1 n, f2 n]
{-
    Test Expressions:
        1. Let a = "abc", f1 be the reverse function and f2 be the tail function.
        => pair "abc" (reverse) (tail) = [reverse("abc"), tail("abc")] = ["cba", "bc"]
        2. Let a = 100, f1 be even function and f2 be odd function.
        => pair 100 (even) (odd) = [even(100), odd(100)] = [True, False]
-}

-- (c) difference --
difference :: [Int] -> Int
difference (x:xs) = foldl (-) x xs
{-
    Test Expressions:
        1. difference [1,2,3] = difference(1:[2,3]) = foldl (-) 1 [2,3] = 1 - 2 - 3 = -4
        2. difference [10,3,2] = difference(10:[3,2]) = foldl (-) 10 [3,2] = 10 - 3 - 2 = 5
-}
-- (d) imp --
imp :: (Int-> Bool) -> (Int->Int) -> (Int->Int) -> Int -> (Int -> Int)
imp f1 f2 f3 a      | f1 a = (*)2
                    | f2 a > 100 = (+)100
                    | f3 a < 99 = (-)99
{-
    Test Expressions:
        1. Let f1 be even function, f2 be a (*5), f3 be a (+20) and a = 59, then f3 will be evaluated and imp will return
           a function (-)99.
        2. Let f1, f2 and f3 be the same as above and a = 100, then f1 will be evaluated 
           and imp will return a function (*)2
-}
