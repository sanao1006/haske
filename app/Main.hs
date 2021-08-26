-- -------------------------------------------------------------------------
-- 　　　　　　ﾊ,,ﾊ
-- 　　　　　（ ﾟωﾟ )　　WAお断りします
-- 　　　　／　　 　＼
-- 　 ((⊂ 　） 　 ﾉ＼つ))
-- 　　　　　（＿⌒ヽ
-- 　　　　　　ヽ ﾍ　}
-- 　　　ε≡Ξ　ﾉノ ｀J
-- ------------------------------------------------------------------------
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
-- {-# LANGUAGE NegativeLiterals #-}
-- {-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -O0 #-}
{-# OPTIONS_GHC -O2 #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE UndecidableInstances#-}

module Main where
import qualified Control.Monad                 as CM
import qualified Control.Monad.Primitive       as Prim
import qualified Control.Monad.ST              as ST
import qualified Data.Array.IArray             as A
import qualified Data.Array.IO                 as AIO
import qualified Data.Array.ST                 as AST
import qualified Data.Array.Unboxed            as AU
import qualified Data.Bits                     as Bits
import qualified Data.ByteString.Char8         as BS
import qualified Data.ByteString.Builder       as BI
import qualified Data.Char                     as Char
import qualified Data.Complex                  as Comp
import qualified Data.Foldable                 as Foldable
import qualified Data.Function                 as Func
import qualified Data.Heap                     as Heap
import qualified Data.IORef                    as IO
import qualified Data.IntPSQ                   as PSQ
import qualified Data.Ix                       as Ix
import qualified Data.List                     as L
import qualified Data.Map.Strict               as Map
import qualified Data.Maybe                    as May
import qualified Data.Primitive.MutVar         as MutVar
import qualified Data.Proxy                    as Proxy
import qualified Data.Ratio                    as Ratio
import qualified Data.STRef                    as STRef
import qualified Data.Sequence                 as Seq
import qualified Data.Set                      as Set
import qualified Data.Tree                     as Tree
import qualified Data.Vector                   as V
import qualified Data.Vector.Algorithms.Merge  as VAM
import qualified Data.Vector.Algorithms.Radix  as VAR
import qualified Data.Vector.Algorithms.Search as VAS
import qualified Data.Vector.Algorithms.Intro  as VAI
import           Data.Vector.Generic           ((!))
import qualified Data.Vector.Generic           as VG
import qualified Data.Vector.Generic.Mutable   as VGM
import qualified Data.Vector.Mutable           as VM
import qualified Data.Vector.Unboxed.Base      as VUB
import qualified Data.Vector.Unboxing          as VU
import qualified Data.Vector.Unboxing.Mutable  as VUM
import qualified Debug.Trace                   as Trace
import qualified GHC.TypeNats                  as TypeNats
import qualified Test.QuickCheck               as QC
import qualified Data.Vector.Fusion.Bundle     as VFB
import System.IO




readInt = BS.readInt . BS.dropWhile Char.isSpace
readIntList = map readInt . BS.words
-- getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

getIntLists n = CM.replicateM n getIntList

getIntVec n = VU.unfoldrN n (BS.readInt . BS.dropWhile Char.isSpace) <$> BS.getLine

-- getIntVecs n m = VU.replicateM m (getIntVec n)

getVecStr =VU.fromList<$>getLine
getInt = readLn @Int
getIntsPair n = map (\[a,b] -> (a,b)) <$> CM.replicateM n getIntList

getVector f = VU.unfoldr f <$> BS.getLine

getIntVector = getVector readInt
getBSVector = getVector BS.uncons
getInt2Tuple = (\vec -> (vec VU.! 0, vec VU.! 1)) <$> getIntVector
getInt3Tuple = (\vec -> (vec VU.! 0, vec VU.! 1, vec VU.! 2)) <$> getIntVector


gs :: IO String
gs=getLine

gn=map (read . BS.unpack) . BS.words <$> BS.getLine ::IO[Int]

gg :: Int -> IO (V.Vector (LV Int))
gg n=V.replicateM n (g @(LV Int))
g::ReadBS a => IO a
g=readBS<$>BS.getLine
gl :: (VG.Vector v a, ReadBS a) => Int -> IO (v a)
gl n = VG.replicateM n g
---------------------------------------------------------

-- main=getContents>>=(print.sum).map read.lines
-- main = do
--   putStrLn "なんか入力して"
--   abc<-do
--     as<-lines<$>BS.getContents
--     let a=map(++ "www")as
--     return a
--   BS.putStr  abc
main = do
  putStrLn "今日の日付入力してください"
  date <- getLine 
  hout <- openFile (date++".txt") WriteMode
  b <- gs
  hPutStrLn hout b
  hClose hout

  

-----------------------------------------------------------

digSum k = sum [read [c] | c <- k]

segments :: [a] -> [[a]]
segments = concat . scanr (\a b -> [a] : map (a:) b) []

lstrip [] = []
lstrip xs'@(x:xs) | x == ' '  = lstrip xs
                  | otherwise = xs'
rstrip = reverse . lstrip . reverse
strip = lstrip . rstrip

replaceAt :: Int -> a -> [a] -> [a]
replaceAt _ _ []     = []
replaceAt 0 y (_:xs) = y : xs
replaceAt n y (x:xs) = x : replaceAt (n - 1) y xs

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


{-# INLINE vLength #-}
vLength :: (VG.Vector v e) => v e -> Int
vLength = VFB.length . VG.stream

type Moji = BS.ByteString
type LV   = V.Vector
type SV   = VU.Vector

get :: ReadBS a => IO a
get = readBS <$> BS.getLine

-- | ex) getLn @Int @VU.Vector n, getLn @[Int] @V.Vector n
getLines :: ReadBSLines a => Int -> IO a
getLines n = readBSLines . BS.unlines <$> CM.replicateM n BS.getLine

-- | 改行なし出力
output :: ShowBS a => a -> IO ()
output = BS.putStr . showBS

-- | 改行なし出力
outputLines :: ShowBSLines a => a -> IO ()
outputLines = BS.putStr . showBSLines

-- | 改行あり出力
pri :: ShowBS a => a -> IO ()
pri = BS.putStrLn . showBS

-- | 改行あり出力
printLn :: ShowBSLines a => a -> IO ()
printLn = BS.putStrLn . showBSLines

---------------
-- Read/Show --
---------------

-- | BS版Read
class ReadBS a where
  readBS :: BS.ByteString -> a

class ShowBS a where
  showBS :: a -> BS.ByteString

instance ReadBS Int where
  readBS s = case BS.readInt s of
    Just (x, _) -> x
    Nothing     -> error "readBS :: BS -> Int"

instance ReadBS Integer where
  readBS = fromIntegral . (readBS @Int)

instance ReadBS Double where
  readBS = read . BS.unpack

instance ReadBS BS.ByteString where
  readBS = id

instance (ReadBS a, VU.Unboxable a) => ReadBS (VU.Vector a) where
  readBS = readVec

instance (ReadBS a) => ReadBS (V.Vector a) where
  readBS = readVec

instance ReadBS a => ReadBS [a] where
  readBS = map readBS . BS.words

instance (ReadBS a, ReadBS b) => ReadBS (a, b) where
  readBS (BS.words -> [a, b]) = (readBS a, readBS b)
  readBS _
    = error "Invalid Format :: readBS :: BS -> (a, b)"

instance (ReadBS a, ReadBS b, ReadBS c) => ReadBS (a, b, c) where
  readBS (BS.words -> [a, b, c]) = (readBS a, readBS b, readBS c)
  readBS _ = error "Invalid Format :: readBS :: BS -> (a, b, c)"

instance (ReadBS a, ReadBS b, ReadBS c, ReadBS d) => ReadBS (a, b, c, d) where
  readBS (BS.words -> [a, b, c, d])
    = (readBS a, readBS b, readBS c, readBS d)
  readBS _
    = error "Invalid Format :: readBS :: BS -> (a, b, c, d)"

instance ShowBS Int where
  showBS = BS.pack . show

instance ShowBS Integer where
  showBS = BS.pack . show

instance ShowBS Double where
  showBS = BS.pack . show

instance ShowBS BS.ByteString where
  showBS = id

instance (ShowBS a, VU.Unboxable a) => ShowBS (VU.Vector a) where
  showBS = showVec

instance (ShowBS a) => ShowBS (V.Vector a) where
  showBS = showVec

instance ShowBS a => ShowBS [a] where
  showBS = BS.unwords . map showBS

instance (ShowBS a, ShowBS b) => ShowBS (a, b) where
  showBS (a, b) =
    showBS a
    `BS.append`
    " "
    `BS.append`
    showBS b

instance (ShowBS a, ShowBS b, ShowBS c) => ShowBS (a, b, c) where
  showBS (a, b, c) =
    showBS a
    `BS.append`
    " "
    `BS.append`
    showBS b
    `BS.append`
    " "
    `BS.append`
    showBS c

instance (ShowBS a, ShowBS b, ShowBS c, ShowBS d) => ShowBS (a, b, c, d) where
  showBS (a, b, c, d) =
    showBS a
    `BS.append`
    " "
    `BS.append`
    showBS b
    `BS.append`
    " "
    `BS.append`
    showBS c
    `BS.append`
    " "
    `BS.append`
    showBS d

readVec :: (VG.Vector v a, ReadBS a) => BS.ByteString -> v a
readVec = VG.fromList . readBS

showVec :: (VG.Vector v a, ShowBS a) => v a -> BS.ByteString
showVec = showBS . VG.toList

class ReadBSLines a where
  readBSLines :: BS.ByteString -> a

class ShowBSLines a where
  showBSLines :: a -> BS.ByteString

instance ReadBS a => ReadBSLines [a] where
  readBSLines = map readBS . BS.lines

instance (ReadBS a, VU.Unboxable a) => ReadBSLines (VU.Vector a) where
  readBSLines = readVecLines

instance ReadBS a => ReadBSLines (V.Vector a) where
  readBSLines = readVecLines

instance ReadBSLines BS.ByteString where
  readBSLines = id

instance ShowBS a => ShowBSLines [a] where
  showBSLines = BS.unwords . map showBS

instance (ShowBS a, VU.Unboxable a) => ShowBSLines (VU.Vector a) where
  showBSLines = showVecLines

instance ShowBS a => ShowBSLines (V.Vector a) where
  showBSLines = showVecLines

instance ShowBSLines BS.ByteString where
  showBSLines = id

readVecLines :: (VG.Vector v a, ReadBS a) => BS.ByteString -> v a
readVecLines = VG.fromList . readBSLines

showVecLines :: (VG.Vector v a, ShowBS a) => v a -> BS.ByteString
showVecLines = showBSLines . VG.toList