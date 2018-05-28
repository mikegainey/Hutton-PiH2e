-- Graham Hutton: Programming in Haskell
-- Abstract Machine, extended example from chapter 8, page 106

data Expr = Val Int | Add Expr Expr
  deriving (Show)

ex1 = (Add (Add (Val 2) (Val 3)) (Val 4))

data Op = EVAL Expr | ADD Int

type Cont = [Op]

eval :: Expr -> Cont -> Int
eval (Val n) c = exec c n
eval (Add x y) c = eval x (EVAL y : c)

exec :: Cont -> Int -> Int
exec [] n = n
exec (EVAL y : c) n = eval y (ADD n : c)
exec (ADD n : c) m = exec c (n+m)

value :: Expr -> Int
value e = eval e []

{-
value (Add (Add (Val 2) (Val 3)) (Val 4))
eval (Add (Add (Val 2) (Val 3)) (Val 4)) []
eval (Add (Val 2) (Val 3)) [EVAL (Val 4)]
eval (Val 2) [EVAL (Val 3), EVAL (Val 4)]
exec [EVAL (Val 3), EVAL (Val 4)] 2
eval (Val 3) [ADD 2, EVAL (Val 4)]
exec [ADD 2, EVAL (Val 4)] 3
exec [EVAL (Val 4)] 5
eval (Val 4) [ADD 5]
exec [ADD 5] 4
exec [] 9
9
-}

-- It works, but I don't understand it very well.
