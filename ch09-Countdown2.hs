-- Graham Hutton: Programming in Haskell
-- Chapter 9, Countdown.hs, extended programming exercise
-- end-of-chapter problem #6

{- 6. Modify the final program to:

6a. allow the use of exponentiation in expressions; (done)

6b. produce the nearest solutions if no exact solution is possible; (done -- use solutions'')

6c. order the solutions using a suitable measure of simplicity; (done -- use solutions''')  -}

main :: IO ()
main = print (solutions' [1,3,7,10,25,50] 765)

data Op = Add | Sub | Mul | Div | Exp

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
  show Exp = "^"

valid :: Op -> Int -> Int -> Bool
valid Add x y = x <= y
valid Sub x y = x > y
valid Mul x y = x /= 1 && y /= 1 && x <= y
valid Div x y = y >= 1 && x `mod` y == 0
valid Exp x y = x /= 1 && y > 1

apply :: Op -> Int -> Int -> Int
apply Add x y = x+y
apply Sub x y = x-y
apply Mul x y = x*y
apply Div x y = x `div` y
apply Exp x y = x^y

data Expr = Val Int | App Op Expr Expr

instance Show Expr where
  show (Val n)     = show n
  show (App o l r) = brak l ++ show o ++ brak r
    where brak (Val n) = show n
          brak e       = "(" ++ show e ++ ")"

values :: Expr -> [Int]
values (Val n)     = [n]
values (App _ l r) = values l ++ values r

eval :: Expr -> [Int]
eval (Val n)     = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l,
                                  y <- eval r,
                                  valid o x y]

subs :: [a] -> [[a]]
subs []     = [[]]
subs (x:xs) = yss ++ map (x:) yss
  where yss = subs xs

interleave :: a -> [a] -> [[a]]
interleave x []     = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

perms :: [a] -> [[a]]
perms []     = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

choices :: [a] -> [[a]]
choices = concat . map perms . subs

solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = elem (values e) (choices ns) && (eval e == [n])

split :: [a] -> [([a],[a])]
split []     = []
split [_]    = []
split (x:xs) = ([x], xs) : [(x:ls, rs)| (ls,rs) <- split xs]

exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs xs = [ e | (ls,rs) <- split xs,
                       l <- exprs ls,
                       r <- exprs rs,
                       e <- combine l r]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

ops :: [Op]
ops = [Add, Sub, Mul, Div, Exp]

solutions :: [Int] -> Int -> [Expr]
solutions ns ans = [e | ns' <- choices ns, e <- exprs ns', eval e == [ans]]

-- combining generation and evaluation: 10x faster

type Result = (Expr, Int)

results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n, n) | n > 0]
results ns = [res | (ls,rs) <- split ns,
                         lx <- results ls,
                         ry <- results rs,
                         res <- combine' lx ry]

combine' :: Result -> Result -> [Result]
combine' (l,x) (r,y) =
  [(App o l r, apply o x y) | o <- ops, valid o x y]

solutions' :: [Int] -> Int -> [Expr]
solutions' ns n =
  [e | ns' <- choices ns, (e,m) <- results ns', m == n]

-- 6b. produce the nearest solutions if no exact solution is possible;

data Solutions = Exact [Expr] | OneOff [Expr] | Nada
  deriving (Show)

solutions'' :: [Int] -> Int -> Solutions
solutions'' ns n
  | length exact  >= 1 = Exact exact
  | length oneOff >= 1 = OneOff oneOff
  | otherwise          = Nada
  where exact  = [e | ns' <- choices ns, (e,m) <- results ns', m == n]
        oneOff = [e | ns' <- choices ns, (e,m) <- results ns', m >= (n-1) && m <= (n+1)]

-- 6c. order the solutions using a suitable measure of simplicity; -}

-- sort by the number of values in the expressions
-- This "suitable measure of simplicity" isn't very useful because even without it, the list of expressions is almost
-- sorted this way anyway.
sortExprs :: [Expr] -> [Expr]
sortExprs [] = []
sortExprs (e:es) = sortExprs less ++ [e] ++ sortExprs more
  where less = [ x | x <- es, (length . values) x <= (length . values) e]
        more = [ y | y <- es, (length . values) y > (length . values) e]

solutions''' :: [Int] -> Int -> [Expr]
solutions''' ns n =
  sortExprs [e | ns' <- choices ns, (e,m) <- results ns', m == n]
