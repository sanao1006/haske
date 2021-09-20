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
import Data.Bool
import Control.Applicative
import Data.Ord
import Control.Monad.State
import qualified Control.Monad                 as CM
import qualified Control.Monad.Primitive       as Prim
import qualified Control.Monad.ST              as ST
import qualified Data.Array             as A
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
import qualified Data.IntMap                   as MI
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

mod' = 1000000007


readInt = BS.readInt . BS.dropWhile Char.isSpace
readIntList = map readInt . BS.words
-- getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

getIntLists n = CM.replicateM n getIntList

getIntVec n = VU.unfoldrN n (BS.readInt . BS.dropWhile Char.isSpace) <$> BS.getLine

getIntVecs n m = CM.replicateM m (getIntVec n)

getVecStr =VU.fromList<$>getLine
getInt = readLn @Int
getIntsPair n = map (\[a,b] -> (a,b)) <$> CM.replicateM n getIntList

getVector f = VU.unfoldr f <$> BS.getLine

getIntVector = getVector readInt
getBSVector = getVector BS.uncons
getInt2Tuple = (\vec -> (vec VU.! 0, vec VU.! 1)) <$> getIntVector
getInt3Tuple = (\vec -> (vec VU.! 0, vec VU.! 1, vec VU.! 2)) <$> getIntVector

ggg s= May.fromJust$L.uncons s

gs :: IO String
gs=getLine

gn=map (read . BS.unpack) . BS.words <$> BS.getLine ::IO[Int]

gg :: Int -> IO (V.Vector (LV Int))
gg n=V.replicateM n (g @(LV Int))


-- | ex) getLn @Int @VU.Vector n, getLn @[Int] @V.Vector n
gl :: (ReadBS a, VG.Vector v a) => Int -> IO (v a)
gl n = VG.replicateM n g

-- | ex) get @Int, get @(VU.Vector) ..
g:: ReadBS a => IO a
g = readBS <$> BS.getLine

fix :: (a -> a) -> a
fix f = let x = f x in x

---------------------------------------------------------


main = do
  putStr "yeah"
---------------------------------------------------------

each :: (t1 -> a) -> (t2 -> b) -> (t1, t2) -> (a, b)
each f g (x,y)=(f x,g y)
digSum k=sum[read[c]|c<-k]



replaceAt :: Int -> a -> [a] -> [a]
replaceAt _ _ []     = []
replaceAt 0 y (_:xs) = y : xs
replaceAt n y (x:xs) = x : replaceAt (n - 1) y xs


{-# INLINE vLength #-}
vLength :: (VG.Vector v e) => v e -> Int
vLength = VFB.length . VG.stream

type Moji = BS.ByteString
type LV   = V.Vector
type SV   = VU.Vector


---------------
-- Read/Show --
---------------

class ReadBS a where
  readBS :: BS.ByteString -> a

  {-# MINIMAL readBS #-}
-- ^ ByteString版Read

class ShowBS a where
  showBS :: a -> BS.ByteString

  {-# MINIMAL showBS #-}


instance ReadBS Int where
  readBS s = case BS.readInt s of
    Just (x, _) -> x
    Nothing     -> error "readBS :: ByteString -> Int"

instance ReadBS Integer where
  readBS = fromInteger . readBS

instance ReadBS Double where
  readBS = read . BS.unpack

instance ReadBS BS.ByteString where
  readBS = id

instance (ReadBS a, VU.Unboxable a) => ReadBS (VU.Vector a) where
  readBS s = VG.fromList . map readBS . BS.words $ s

instance (ReadBS a) => ReadBS (V.Vector a) where
  readBS s = VG.fromList . map readBS . BS.words $ s

instance ReadBS a => ReadBS [a] where
  readBS s = map readBS . BS.words $ s

instance (ReadBS a, ReadBS b) => ReadBS (a, b) where
  readBS (BS.words -> [a, b]) = (readBS a, readBS b)
  readBS _ = error "Invalid Format :: readBS :: ByteString -> (a, b)"

instance (ReadBS a, ReadBS b, ReadBS c) => ReadBS (a, b, c) where
  readBS (BS.words -> [a, b, c]) = (readBS a, readBS b, readBS c)
  readBS _ = error "Invalid Format :: readBS :: ByteString -> (a, b)"

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
  showBS = BS.pack . unwords . map (BS.unpack . showBS)

instance (ShowBS a, ShowBS b) => ShowBS (a, b) where
  showBS (a, b) = showBS a `BS.append` " " `BS.append` showBS b

instance (ShowBS a, ShowBS b, ShowBS c) => ShowBS (a, b, c) where
  showBS (a, b, c) = showBS a `BS.append` " " `BS.append` showBS b
    `BS.append` showBS c

showVec :: (VG.Vector v a, ShowBS a) => v a -> BS.ByteString
showVec xs
  | VG.null xs = ""
  | otherwise = BS.concat [f i | i <- [0 .. 2 * VG.length xs - 2]]
  where
  f i
    | even i = showBS $ xs ! (i `div` 2)
    | otherwise = " "


while :: Monad m => m Bool -> m ()
while f = f >>= \frag -> CM.when frag $ while f