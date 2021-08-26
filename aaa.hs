----usage----------
-- e <- gl @LV @(Int, Int, Int, Int)  d
-- xs<-gl @LV @(Moji)  n
-- VU.zip3 w (VU.drop 1 w) (VU.drop 2 w) 3つずつにわける
--  V.modify (VAM.sortBy (\x y -> compare (snd y) (snd x)))
--  xs<- CM.replicateM n $ do
--         [a,b]<-words <$> getLine
--         return (a,read b :: Int)

-- solve n b =VU.create $ do
--     a<-VUM.new n
--     VUM.write a 0 $b VU.! 0
--     VU.forM_ (VU.fromList[1..n-2]) $ \i ->do
--         VUM.unsafeWrite a i $ min(b VU.! (i-1))(b VU.! i)
--     VUM.write a (n-1) $ b VU.! (n-2)
--     return a

-- main = do 
--     var <- VUM.replicate 1 (0 :: Int)
--     forM_ [1..100] $ \i -> do
--       when (even i) $ do
--         VUM.modify var (+ i) 0
--     value <- VUM.read var 0
--     print value
-- let result = VU.modify VAI.sort a
-- a= VU.enumFromN (1::Int) n
-- let a= VU.iterateN n(\(i, c) -> (i*2,succ c)) (1 :: Int, 'a')

    -- items <- replicateM n $ do
    --   [w,v] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$>BS.getLine
    --   return (w, fromIntegral v :: Int64)
    -- print$VU.fromList $map(fst) items

--------------------
-- main=do
--  n<-readLn
--  putStrLn$foldl(++)""$take n$repeat "ACL"

-- main=interact$takeWhile(/='.')

--   [a,b,c,d] <- getLine ４つの数字文字列を半分で分割
--   let x = read [a,b] :: Int
--       y = read [c,d] :: Int

--    var <- IO.newIORef (0 :: Int)
    -- forM_ [1..8] $ \i -> do
    --   when (even i) $ do
    --     IO.modifyIORef' var (+ i)
    -- value <- IO.readIORef var
    -- when (value >=30) $ do
    --     putStr "otsu!!"

-- main=getLine>>=print.foldr(max.read)0.words最大
-- import Data.List;main=print.length.filter(isPrefixOf"ZONe").tails=<<getLine
-- VU.thaw :: (VU.Unbox a, PrimMonad m) =>
-- VU.Vector a -> m (VUM.MVector (PrimState m) a
-- [a,b] <- map read . words <$> getLine :: IO [Int]
-- main=interact$f.words;f[a,b]|a==b="H"|0<1="D"
-- main=interact$f.map read.words;f[x,a,b]|(a-x)^2<(b-x)^2="A"|0<1="B"
-- main=getLine>>=print.(\[a,b]->if mod a b==0 then a+b else b-a).map read.words
-- main=interact$show.f.map read.words;f[a,b,c]=2*(a*b+b*c+c*a) 数値かけるだけ
-- main=(!!)<$>getLine<*>(pred<$>readLn)>>=putChar
-- main=interact$f.sum.map read.words;f x|x<10=show x|0<1="error"
    -- putStrLn$show(div (a+b) 2)++" "++show(div (a-b) 2)
    -- main=do(c:s)<-getLine;putStr$c:shows(length s-1)[last s]
-- main=interact(\s->show$sum[1|'2'<-s])
-- main=getLine>>=print.foldr(max.read)0.words 最大値1つ
-- solve=length.filter(\s->take 3 s=="ABC").tails
-- main=gcd<$>readLn<*>readLn>>=print
-- main=(-).(*2)<$>readLn<*>readLn>>=print
-- main=interact f;f s|and[c`elem`s|c<-"abc"]="Yes"|0<1="No" 文字列含む
-- main=getLine>>map read.words<$>getLine>>=print.((-)<$>maximum<*>minimum)
-- main=(-10)<$>((sum)<$>map read.words<$>getLine)<*>(sum<$>map read.words<$>getLine)
-- main=readLn>>=print.(-1+)
-- main=getContents.map(map read.words).lines>>= \[a,b]->print$(sum a)+(sum b)


-- main :: IO ()
-- main = do
--   n<-g @Int
--   xy<-gl @SV @(Int,Int) n
--   let ans = sum [f (xy VU.! i) (xy VU.! j)|i <- [0..n-2], j <- [i+1..n-1]]
--   print ans
-- f (x1,y1) (x2,y2)
--     | x1 == x2 = 0
--     | x2 - x1 > 0 = if x1 - x2 <= y2 -y1 && y2 - y1 <= x2 - x1 then 1 else 0
--     | x2 -x1 < 0 = if x2 -x1 <= y2 - y1 && y2 - y1 <= x1 - x2 then 1 else 0

------------
-- ModInt --
------------

newtype Mod a (p :: TypeNats.Nat) = ModInt a deriving (Eq, Show)

instance VUB.Unbox a => VU.Unboxable (Mod a p) where
  type Rep (Mod a p) = a

instance ShowBS a => ShowBS (Mod a p) where
  showBS (ModInt x) = showBS x

instance (TypeNats.KnownNat p, Integral a) => Num (Mod a p) where
  (ModInt x) + (ModInt y) = ModInt $ (x + y) `mod` p where
    p = fromIntegral $ TypeNats.natVal (Proxy.Proxy :: Proxy.Proxy p)
  (ModInt x) * (ModInt y) = ModInt $ (x * y) `mod` p where
    p = fromIntegral $ TypeNats.natVal (Proxy.Proxy :: Proxy.Proxy p)
  negate (ModInt x) = ModInt $ - x `mod` p where
    p = fromIntegral $ TypeNats.natVal (Proxy.Proxy :: Proxy.Proxy p)
  abs = id
  signum _ = 1
  fromInteger n = ModInt $ fromInteger n `mod` p where
    p = fromIntegral $ TypeNats.natVal (Proxy.Proxy :: Proxy.Proxy p)

instance (TypeNats.KnownNat p, Integral a) => Fractional (Mod a p) where
  recip (ModInt n)
    | gcd n p /= 1 = error "recip :: Mod a p -> Mod a p : The inverse element does not exist."
    | otherwise = ModInt . fst $ extendedEuc n (-p)
    where
    p = fromIntegral $
      TypeNats.natVal (Proxy.Proxy :: Proxy.Proxy p)
  fromRational r = ModInt n / ModInt d where
    n = fromInteger $ Ratio.numerator r
    d = fromInteger $ Ratio.denominator r

----------------------------
-- Coordinate Compression --
----------------------------

comp :: (VU.Unboxable  a, Ord a) => VU.Vector a -> (VU.Vector a, VU.Vector Int)
comp xs = (vals, comped) where
  vals = VU.uniq . VU.modify VAM.sort $ xs
  comped = ST.runST $ VU.thaw vals >>= \v -> VU.mapM (VAS.binarySearch v) xs

uncomp :: VU.Unboxable  a => VU.Vector a -> VU.Vector Int -> VU.Vector a
uncomp vals = VU.map (vals !)

------------
-- InfInt --
------------

newtype Inf a = Inf (V.Vector a) deriving (Eq)
-- ↓バカ実装(?)
instance ReadBS a => ReadBS (Inf a) where
  readBS = Inf . readBS

instance ShowBS a => ShowBS (Inf a) where
  showBS (Inf xs) = showBS xs

instance (Num a, Ord a) => Num (Inf a) where
  (Inf xs) + (Inf ys) = infMold . Inf $ zs
    where
    lx = V.length xs - 1
    ly = V.length ys - 1
    zs = V.map f [0 .. max lx ly]
    f i = xs .! i + ys .! i
  (Inf xs) * (Inf ys) = infMold . Inf $ zs
    where
    lx = V.length xs - 1
    ly = V.length ys - 1
    zs = V.map f [0 .. lx + ly]
    f i = V.sum . V.map g $ [0 .. i]
      where
      g j = xs .! j * ys .! (i - j)
  negate (Inf xs) = Inf . V.map negate $ xs
  abs n = if n >= 0 then n else - n
  signum n = if n >= 0 then 1 else - 1
  fromInteger n = Inf [fromInteger n]

instance (Num a, Ord a) => Ord (Inf a) where
  compare (Inf xs) (Inf ys)
    | l == -1 = EQ
    | xs .! l == ys .! l = compare (Inf (V.init xs)) (Inf (V.init ys))
    | otherwise = compare (xs .! l) (ys .! l)
    where
    l = max (V.length xs) (V.length ys) - 1

(.!) :: (VG.Vector v p, Num p) => v p -> Int -> p
xs .! i
  | i >= 0 && i < VG.length xs = xs ! i
  | otherwise = 0

infinity :: Num a => Inf a
infinity = Inf [0, 1]

fromInf :: (Eq a, Num a) => Inf a -> Maybe a
fromInf (Inf xs)
  | V.length xs == 1 = Just $ xs ! 0
  | otherwise = Nothing

infMold :: (Num a, Eq a) => Inf a -> Inf a
infMold (Inf xs) = Inf . dropWhileRev (== 0) $ xs

dropWhileRev :: VG.Vector v a => (a -> Bool) -> v a -> v a
dropWhileRev f xs
  | VG.null xs = VG.empty
  | f x = dropWhileRev f xs'
  | otherwise = xs
  where
  x = VG.last xs
  xs' = VG.init xs

------------------
-- Disjoint Set --
------------------

type DisjointSet = VU.Vector Int
data DisjointSetM m = DSet
  {dsParents :: VUM.MVector m Int, dsDepths ::VUM.MVector m Int}

dsFromEdges :: Int -> VU.Vector (Int, Int) -> VU.Vector Int
dsFromEdges n edges = VU.create do
  ds <- newDSet n
  VU.forM_ edges $ uncurry (union ds)
  return $ dsParents ds

newDSet :: Prim.PrimMonad m => Int -> m (DisjointSetM (Prim.PrimState m))
newDSet n = DSet <$> VU.thaw (VU.generate n id) <*> VUM.replicate n 1

root :: DisjointSet -> Int -> Int
root xs i
  | xs ! i == i = i
  | otherwise = root xs $ xs ! i

find :: DisjointSet -> Int -> Int -> Bool
find xs i j = root xs i == root xs j

-- | ルートを調べる時につなぎ直す
rootM :: Prim.PrimMonad m => DisjointSetM (Prim.PrimState m) -> Int -> m Int
rootM ds i = VUM.read (dsParents ds) i >>= \p -> if p == i
  then return i
  else rootM ds p >>= \r -> VUM.write (dsParents ds) i r >> return r

union :: Prim.PrimMonad m =>
  DisjointSetM (Prim.PrimState m) -> Int -> Int -> m ()
union ds i j = do
  rooti <- rootM ds i
  rootj <- rootM ds j
  depi <- VUM.read (dsDepths ds) rooti
  depj <- VUM.read (dsDepths ds) rootj
  if
    | depi == depj -> VUM.modify (dsDepths ds) (+ 1) rooti >>
    VUM.write (dsParents ds) rootj rooti
    | depi > depj -> VUM.write (dsParents ds) rootj rooti
    | otherwise  -> VUM.write (dsParents ds) rooti rootj

findM :: Prim.PrimMonad m =>
  DisjointSetM (Prim.PrimState m) -> Int -> Int -> m Bool
findM ds i j = (==) <$> rootM ds i <*> rootM ds j

----------------------------
-- Monadic Priority Queue --
----------------------------

-- 優先度付きキュー(IntPSQ)のモナディックな実装(いらないかも)
type PriorityQueue p = PSQ.IntPSQ p ()
type MPriorityQueue s p = MutVar.MutVar s (PriorityQueue p)

mpqNew :: (Prim.PrimMonad m, Ord p) => m (MPriorityQueue (Prim.PrimState m) p)
mpqNew = MutVar.newMutVar PSQ.empty

mpqMinView ::  (Prim.PrimMonad m, Ord p) => MPriorityQueue (Prim.PrimState m) p -> m (Maybe Int)
mpqMinView mpq = do
  pq <- MutVar.readMutVar mpq
  case PSQ.minView pq of
    Nothing             -> return Nothing
    Just (n, _, _, pq') -> MutVar.writeMutVar mpq pq' >> return (Just n)

mpqInsert :: (Prim.PrimMonad m, Ord p) => MPriorityQueue (Prim.PrimState m) p -> Int -> p -> m ()
mpqInsert mpq n p = MutVar.modifyMutVar' mpq (PSQ.insert n p ())

-----------
-- Graph --
-----------
type Graph a = V.Vector [(Int, a)]
type UGraph = Graph ()

gEmpty :: Graph a
gEmpty = V.singleton []

gFromEdges :: VU.Unboxable  a => Int -> VU.Vector (Int, Int, a) -> Graph a
gFromEdges n edges = ST.runST do
  v <- VM.replicate n []
  VU.forM_ edges \(i, j, a) -> VM.modify v ((j, a):) i
  V.freeze v

gReverse :: Graph a -> Graph a
gReverse g = ST.runST do
  let
    n = V.length g
  v <- VM.replicate n []
  V.forM_ [0 .. n - 1] \i -> M.forM_ (g ! i) \(j, a) -> VM.modify v ((i, a) :) j
  V.freeze v

dijkstra :: Graph Int -> Int -> V.Vector (Inf Int)
dijkstra g i = ST.runST do
  let
    n = V.length g
  d <- VM.new n
  q <- mpqNew
  V.forM_ [0 .. n - 1] \j -> let dj = if i == j then 0 else infinity in
    VM.write d j dj >> mpqInsert q j dj
  while $ mpqMinView q >>= \case
    Nothing -> return False
    Just u -> do
      dist_u <- VM.read d u
      M.forM_ (g ! u) \(v,len) -> do
        dist_v <- VM.read d v
        let
          alt = dist_u + fromIntegral len
        M.when (dist_v > alt) $ VM.write d v alt >> mpqInsert q v alt
      return True
  V.freeze d

dfs :: Graph a -> Int -> Tree.Tree Int
dfs g i = ST.runST do
  reached <- VUM.replicate (V.length g) False
  let
    loop now = do
      VUM.write reached now True
      nexts <- M.filterM (fmap not . VUM.read reached) . map fst $ g ! now
      childs <- M.mapM loop nexts
      return $ Tree.Node now childs
  loop i

