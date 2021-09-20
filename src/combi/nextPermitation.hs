--順列全探索用

nextPermutation :: Ord a => [a] -> Maybe [a]
nextPermutation xs =
  let
    len = length xs
    revSuffix = findPrefix (reverse xs)
    suffixLen = length revSuffix
    prefixMinusPivot = take (len - suffixLen - 1) xs
    pivot = xs !! (len - suffixLen - 1)
    suffixHead = takeWhile (<= pivot) revSuffix
    newPivot : suffixTail = drop (length suffixHead) revSuffix
    newSuffix = suffixHead ++ (pivot : suffixTail)
  in
    if suffixLen == len
    then Nothing
    else Just (prefixMinusPivot ++ (newPivot : newSuffix))
  where
    findPrefix [] = []
    findPrefix (x:xs) =
      x : (if xs /= [] && x <= head xs then findPrefix xs else [])