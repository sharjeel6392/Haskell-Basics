-- Implementation of Peano (9.a) --
data Peano = Zero | S Peano deriving Show

-- Implementation of add (9.b) --
add :: Peano -> Peano -> Peano
add s Zero = s
add s (S c) = S (add s c)

-- Implementation of mult (9.c) --
mult :: Peano -> Peano -> Peano
mult s Zero = Zero
mult (S s) (S c) = S (add s (mult (S s) c))

-- Implementation of fact (9.d) --
fact :: Peano -> Peano
fact Zero = S Zero
fact (S c) = mult (S c) (fact c)

test :: Peano -> Peano
test Zero = Zero
test (S c) = S (test c)