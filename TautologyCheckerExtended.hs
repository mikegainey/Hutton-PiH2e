-- Graham Hutton: Programming in Haskell
-- Tautology Checker, extended example from chapter 8, page 101
-- end-if-chapter exercise #8, extend to include logical disjunction (Or) and equivalence (Equiv)

-- used to associate variables ('A'..'Z') with Bool values
type Assoc a b = [(a,b)]
type Subst = Assoc Char Bool

data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Or Prop Prop
          | Imply Prop Prop
          | Equiv Prop Prop
          deriving (Show)

find :: Eq k => k -> Assoc k v -> v
find k t = head [ v | (k',v) <- t, k' == k]

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : rmdups (filter (/=x) xs)

p1 :: Prop
p1 = And (Var 'A') (Not (Var 'A'))

p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')

p3 :: Prop
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))

p4 :: Prop
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')

p5 :: Prop
p5 = Or (Var 'A') (Not (Var 'A'))

pand :: Prop
pand = And (Var 'A') (Var 'B')

por :: Prop
por = Or (Var 'A') (Var 'B')

vars :: Prop -> [Char]
vars (Const b)     = []
vars (Var c)       = [c]
vars (Not p)       = vars p
vars (And p q)     = (vars p) ++ (vars q)
vars (Or p q)      = (vars p) ++ (vars q)
vars (Imply p q)   = (vars p) ++ (vars q)
vars (Equiv p q)   = (vars p) ++ (vars q)

eval :: Subst -> Prop -> Bool
eval _ (Const b)   = b
eval s (Var c)     = find c s
eval s (Not p)     = not (eval s p)
eval s (And p q)   = (eval s p) && (eval s q)
eval s (Or p q)    = (eval s p) || (eval s q)
eval s (Imply p q) = (eval s p) <= (eval s q)
eval _ (Equiv p q) = pTruth == qTruth          -- Equiv not tested, but I think it works
  where pTruth = [eval s p | s <- (subst p)]
        qTruth = [eval s q | s <- (subst q)]

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False:) prevBools ++ map (True:) prevBools
  where prevBools = bools (n-1)

subst :: Prop -> [Subst]
subst p = map (zip varlist) (bools (length varlist))
  where varlist = rmdups (vars p)

isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- (subst p)]
