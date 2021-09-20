--エラトステネスの篩

sieveOfEra :: Int -> [Int]
sieveOfEra n
    | n < 2 = []
    | otherwise = sieve [x | x <- [2..n]] n
    where
        sieve :: [Int] -> Int -> [Int]
        sieve [] _ = []
        sieve (x:xs) n
            | x^2 > n = x:xs
            | otherwise = x : sieve [a | a <- xs, mod a x > 0] n
