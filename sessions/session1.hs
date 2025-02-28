--
-- We are going to show how Type Theory works within functional programming language like Haskell. 
--
-- For instance
let s :: Int -> Int = (\n -> n+1)

-- plus : Nat -> Nat -> Nat
-- x, y: Nat |- plus(x, y) = rec(x; (z, w, Sw), y) : Nat
-- x + 0 = x
-- x + Sy = S(x+y)
plus0 :: Int -> Int -> Int
plus0 x 0 = x
plus0 x y = s((plus x (y-1)))

-- rec_C : C -> (N -> C -> C) -> N -> C
data Nat = Z | S Nat deriving (Show)
let rec :: a -> (Nat -> a -> a) -> Nat -> a
rec dZ dS Z = dZ
rec dZ dS (S n') = dS n' (rec dZ dS n')

let plus :: Nat -> Nat -> Nat = \ x y -> rec x (\ z w -> S w) y

plus (S (S Z)) (S (S Z)) -- output: S ( S ( S ( S Z ) ) ) )

-- Ackermann function A : NN times NN -> N
-- A(0, n) := sn
-- A(sm, 0) = 1
-- A(sm, sn) = A(m, A(sm, n))
-- It grows faster than any primitive recursive functions. Functions types can define it, but our original system can not.
ack :: Int -> Int -> Int
ack 0 n = n + 1
ack m 0 = ack(m-1, 1)
ack(m, n) = ack(m-1, ack(m, n-1))
-- Exercise: try to define it without function types
