-- Graham Hutton: Programming in Haskell
-- Abstract Machine, extended example from chapter 8, page 106
-- end-of-chapter exercise #9, extend the abstract machine to support the use of multiplication

data Expr = Val Int | Add Expr Expr | Mul Expr Expr
  deriving (Show)

ex1 = (Add (Add (Val 2) (Val 3)) (Val 4))
ex2 = (Mul (Mul (Val 2) (Val 3)) (Add (Val 4) (Val 5)))

data Op = ADDEVAL Expr | MULEVAL Expr | ADD Int | MUL Int

type Stack = [Op]

eval :: Expr -> Stack -> Int
eval (Val n) s   = exec s n
eval (Add x y) s = eval x (ADDEVAL y : s)
eval (Mul x y) s = eval x (MULEVAL y : s)

exec :: Stack -> Int -> Int
exec [] n           = n
exec (ADDEVAL y : s) n = eval y (ADD n : s)
exec (MULEVAL y : s) n = eval y (MUL n : s)
exec (ADD n : s) m  = exec s (n+m)
exec (MUL n : s) m  = exec s (n*m)

value :: Expr -> Int
value e = eval e []

-- Note: It works, but I don't understand it very well.
