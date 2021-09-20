--部分文字列

segments :: [a] -> [[a]]
segments = concat . scanr (\a b -> [a] : map (a:) b) []
