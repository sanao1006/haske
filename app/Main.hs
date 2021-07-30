-- -------------------------------------------------------------------------
-- 　　　　　　ﾊ,,ﾊ
-- 　　　　　（ ﾟωﾟ )　　WAお断りします
-- 　　　　／　　 　＼
-- 　 ((⊂ 　） 　 ﾉ＼つ))
-- 　　　　　（＿⌒ヽ
-- 　　　　　　ヽ ﾍ　}
-- 　　　ε≡Ξ　ﾉノ ｀J
-- ------------------------------------------------------------------------
-- {-# LANGUAGE BangPatterns #-}
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

module Main where

import Control.Applicative ((<$>))
import Control.Arrow
import Data.Array
-- import Prelude hiding (print)
-- import qualified Data.Array as DA
import Control.Monad (replicateM)
import qualified Control.Monad as CM
import qualified Control.Monad.ST as ST
import qualified Data.Array as DA
-- import qualified Data.Array.IArray as A
-- import qualified Data.Array.IO
-- import qualified Data.Array.ST as AST
import qualified Data.Array.Unboxed as AU
import qualified Data.Bits
import qualified Data.ByteString.Char8 as BS
-- import qualified Data.Complex as Comp

-- import Prelude

import Data.Char
import Data.Foldable (foldl')
import qualified Data.Foldable as Foldable
import qualified Data.Function as Func
import qualified Data.Heap as Heap
import Data.IORef
import qualified Data.IntPSQ as PSQueue
import qualified Data.Ix as Ix
import  Data.List
import qualified Data.Map.Strict as Map
-- import Data.Maybe (fromJust)
import qualified Data.Maybe as May
import qualified Data.Proxy as Proxy
import qualified Data.Ratio as Ratio
import qualified Data.STRef as STR
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Tree as Tree
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as VAI
import qualified Data.Vector.Algorithms.Merge as VAM
import qualified Data.Vector.Algorithms.Radix as VAR
import qualified Data.Vector.Algorithms.Search as VAS
import Data.Vector.Generic ((!))
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Debug.Trace as Trace
import qualified GHC.TypeNats as TypeNats
import Text.Read (Lexeme (String))
import Prelude

toInt :: Float -> Int
toInt = round

-- 部分文字列
segments :: [a] -> [[a]]
segments = foldr (++) [] . scanr (\a b -> [a] : map (a:) b) []

length' [] = 0
length' (_ : xs) = 1 + length' xs


tuplify2 (x:y:_) = (x,y)
tuplify2 _ = undefined

--Input functions with ByteString
readInt :: IO [Int]
readInt=map(fst.May.fromJust.BS.readInt).BS.words<$>BS.getLine

readInts :: Int -> IO [[Int]]
readInts n = replicateM n readInt

readInteger :: IO [Integer]
readInteger =map(fst.May.fromJust.BS.readInteger).BS.words<$>BS.getLine

readIntegers :: Int -> IO [[Integer]]
readIntegers n = replicateM n readInteger

readString :: IO [String]
readString = map BS.unpack . BS.words <$> BS.getLine

readStrings :: Int -> IO [[String]]
readStrings n = replicateM n <$> readString

check::[Char]->[Char]->Bool
check [][]              =False
check _[]               =False
check []_               =False
check(x:xs)(y:ys)
 | y==x                 =True
 | otherwise            =check xs (y:ys)


-- putStr$unwords$map show a リスト内の数字空白区切り全展開
-- vector用
-- --  n <- readLn
-- vec2= VU.unfoldrN n (BS.readInt . BS.dropWhile isSpace)<$> BS.getLine


--   [a,b,c,d] <- getLine ４つの数字文字列を半分で分割
--   let x = read [a,b] :: Int
--       y = read [c,d] :: Int

-- factList :: Integer -> [Integer]
-- factList = scanl (*) 1 [1..1000]

-- binom :: Int -> Int -> Integer
-- binom n k = factList !! n `div` (factList !! (n - k) * factList !! k)
--縦にn回入力

-- VU.thaw :: (VU.Unbox a, PrimMonad m) =>
-- VU.Vector a -> m (VUM.MVector (PrimState m) a
-- [a,b] <- map read . words <$> getLine :: IO [Int]
-- main=getLine>>=print.foldr(max.read)0.words
-- main=interact$f.words;f[a,b]|a==b="H"|0<1="D"
-- main=interact f;f(a:_:b:_)|a==b="H"|0<1="D"
-- main=interact$f.map read.words;f[x,a,b]|(a-x)^2<(b-x)^2="A"|0<1="B"
-- main=getLine>>=print.(\[a,b]->if mod a b==0 then a+b else b-a).map read.words
-- main=interact$show.f.map read.words;f[a,b,c]=2*(a*b+b*c+c*a) 数値かけるだけ
-- main=(!!)<$>getLine<*>(pred<$>readLn)>>=putChar
-- main=readLn>>=print.(*2)
-- main=interact$show.(`mod` 24).sum.map read.words
-- main=interact$f.sum.map read.words;f x|x<10=show x|0<1="error"
-- main=getContents>>=print.f.map read.words;f[a,b,n]=[x|x<-[n..],mod x a+mod x b<1]!!0
    -- putStrLn$show(div (a+b) 2)++" "++show(div (a-b) 2)
    -- main=do(c:s)<-getLine;putStr$c:shows(length s-1)[last s]
-- main=interact(\s->show$sum[1|'2'<-s])

main :: IO ()
main = do
    [n]<-readInt
    print.length$segments s