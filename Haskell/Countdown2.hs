type Value = Int
data Expr = Num Value | App Op Expr Expr deriving Show
data Op = Add | Sub | Mul | Div deriving Show

value :: Expr -> Value
value (Num x) = x
value (App op e1 e2) = apply op (value e1) (value e2)

apply :: Integral a => Op -> a -> a -> a
apply Add = (+)
apply Sub = (-)
apply Mul = (*)
apply Div = div

legal :: Op -> Value -> Value -> Bool
legal Add v1 v2 = (v1 <= v2)
legal Sub v1 v2 = (v2 < v1)
legal Mul v1 v2 = (1 < v1) && (v1 <= v2)
legal Div v1 v2 = (1 < v2) && (v1 `mod` v2 == 0)

unmerges :: [a] -> [([a], [a])]
unmerges [x, y] = [([x], [y]), ([y], [x])]
unmerges (x:xs) =
    [([x], xs), (xs, [x])] ++ concatMap (add x) (unmerges xs)
    where add x (ys, zs) = [(x:ys, zs), (ys, x:zs)]

ops :: [Op]
ops = [Add, Sub, Mul, Div]

combine :: (Expr, Value) -> (Expr, Value) -> [(Expr, Value)]
combine (e1, v1) (e2, v2) =
    [ (App op e1 e2, apply op v1 v2) | op <- ops, legal op v1 v2]

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
    let answer = countdown 831 [1, 3, 7, 10, 25, 50]
    putStrLn $ "answer: " ++ show answer
