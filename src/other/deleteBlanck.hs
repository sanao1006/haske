lstrip [] = []
lstrip xs'@(x:xs) | x == ' '  = lstrip xs
                  | otherwise = xs'
rstrip :: [Char] -> [Char]
rstrip = reverse . lstrip . reverse
strip = lstrip . rstrip