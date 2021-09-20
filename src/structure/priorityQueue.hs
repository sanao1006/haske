import qualified Data.Primitive.MutVar         as MutVar
import qualified Control.Monad.Primitive       as Prim

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
