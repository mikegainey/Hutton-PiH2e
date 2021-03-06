-- Graham Hutton: Programming in Haskell
-- The countdown problem, extended example from chapter 9, page 111
-- to compile: $ ghc -O2 Countdown.hs
-- to run: $ ./Countdown

main :: IO ()
main = print (solutions' [1,3,7,10,25,50] 765)

data Op = Add | Sub | Mul | Div

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"

oldValid :: Op -> Int -> Int -> Bool
oldValid Add _ _ = True
oldValid Sub x y = x > y
oldValid Mul _ _ = True
oldValid Div x y = x `mod` y == 0

-- because 1 + 2 and 2 + 1 are equivalent, use only 1 + 2 (x <= y)
-- because 5 / 1 and 5 are equivalent, don't allow y to be 1
valid :: Op -> Int -> Int -> Bool
valid Add x y = x <= y
valid Sub x y = x > y
valid Mul x y = x /= 1 && y /= 1 && x <= y
valid Div x y = y /= 1 && x `mod` y == 0

apply :: Op -> Int -> Int -> Int
apply Add x y = x+y
apply Sub x y = x-y
apply Mul x y = x*y
apply Div x y = x `div` y

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

split' xs = map (\n -> splitAt n xs) [1..((length xs) - 1)] -- my implementation

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
ops = [Add, Sub, Mul, Div]

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

-- exploiting algebraic properties: improved the valid function
