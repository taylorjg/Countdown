import Data.Maybe (fromJust)

type Value = Int
data Expr = Num Value | App Op Expr Expr
data Op = Add | Sub | Mul | Div deriving Eq

instance Show Expr where
    show (Num x) = show x
    show (App op e1 e2) = "(" ++ show e1 ++ (fromJust $ lookup op opSymbols) ++ show e2 ++ ")"
        where
            opSymbols = [(Add, "+"), (Sub, "-"), (Mul, "*"), (Div, "/")]

unmerges :: [a] -> [([a], [a])]
unmerges [x, y] = [([x], [y])]
unmerges (x:xs) =
    [([x], xs)] ++ concatMap (add x) (unmerges xs)
    where add x (ys, zs) = [(x:ys, zs), (ys, x:zs)]

non :: Op -> Expr -> Bool
non _ (Num _) = True
non op1 (App op2 _ _) = op1 /= op2

comb1 (e1, v1) (e2, v2) =
    (if (non Sub e1 && non Sub e2) then
    [(App Add e1 e2, v1 + v2) | non Add e2] ++ [(App Sub e2 e1, v2 - v1)]
    else []) ++
    (if 1 < v1 && non Div e1 && non Div e2 then
    [(App Mul e1 e2, v1 * v2) | non Mul e2] ++ [(App Div e2 e1, q) | r == 0]
    else [])
    where
        (q, r) = v2 `divMod` v1

comb2 (e1, v1) (e2, v2) =
    [(App Add e1 e2, v1 + v2) | non Sub e1, non Add e2, non Sub e2] ++
    (if 1 < v1 && non Div e1 && non Div e2 then
    [(App Mul e1 e2, v1 * v2) | non Mul e2] ++ [(App Div e1 e2, 1)]
    else [])

combine :: (Expr, Value) -> (Expr, Value) -> [(Expr, Value)]
combine (e1, v1) (e2, v2)
    | v1 < v2 = comb1 (e1, v1) (e2, v2)
    | v1 == v2 = comb2 (e1, v1) (e2, v2)
    | v1 > v2 = comb1 (e2, v2) (e1, v1)

nearest :: Value -> [(Expr, Value)] -> (Expr, Value)
nearest n (ev@(_, v):evs) =
    if d == 0 then ev
    else search n d ev evs
    where d = abs (n - v)

search :: Value -> Value -> (Expr, Value) -> [(Expr, Value)] -> (Expr, Value)
search _ _ ev [] = ev
search n d ev ((e, v):evs)
    | d' == 0 = (e, v)
    | d' < d = search n d' (e, v) evs
    | d' >= d = search n d ev evs
    where d' = abs (n - v)

mkExprs :: [Value] -> [(Expr, Value)]
mkExprs [x] = [(Num x, x)]
mkExprs xs =
    [ ev | (ys, zs) <- unmerges xs,
           ev1 <- mkExprs ys,
           ev2 <- mkExprs zs,
           ev <- combine ev1 ev2 ]

subseqs :: [a] -> [[a]]
subseqs [x] = [[x]]
subseqs (x:xs) =
    xss ++ [x]:map (x:) xss
    where xss = subseqs xs

countdown :: Value -> [Value] -> (Expr, Value)
countdown n = nearest n . concatMap mkExprs . subseqs

main :: IO ()
main = do
    let numbers = [1, 3, 7, 10, 25, 50]
    let answer = countdown 831 numbers
    putStrLn $ show (fst answer) ++ " = " ++ show (snd answer)
