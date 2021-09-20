--約数全列挙

divisor :: Integral a => a -> [a]
divisor n
  | n <= 0 = error
    "divisor : Definition range does not include negative numbers or zero"
  | otherwise = half ++ rev where
  mid = floor . sqrt @Double . fromIntegral $ n
  half = filter ((== 0) . mod n) [1 .. mid]
  rev = reverse . map (n `div`).(if mid ^ (2 :: Int) == n then init else id) $ half

