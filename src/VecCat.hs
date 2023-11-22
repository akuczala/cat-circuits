{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
module VecCat(
    VecCat(..)
) where

import qualified Data.Vector.Sized as V
import GHC.TypeNats
import Control.Arrow (first)
import Data.Bifunctor (bimap)
import Graph

class VecCat k where
    splitHead :: V.Vector (1 + n) a `k` (a, V.Vector n a)
    -- mergeHead
    flatOne :: V.Vector 1 a `k` a
    vanishNone :: V.Vector 0 a `k` ()
    fromPair :: (a, a) `k` V.Vector 2 a
    toPair :: V.Vector 2 a `k` (a, a)

instance VecCat (->) where
    splitHead v = first V.head (V.splitAt v)
    flatOne  = V.head
    vanishNone _ = ()
    fromPair = V.fromTuple
    toPair :: V.Vector 2 a -> (a, a)
    toPair v = bimap flatOne flatOne (V.splitAt v)

instance VecCat Graph where
  splitHead = Graph (\(VecP v) -> f v)
    where
        f v = return (PairP x (VecP y))
            where (x, y) = splitHead v
  flatOne = Graph(\(VecP v) -> return $ flatOne v)
  vanishNone = Graph(\(VecP _) -> return UnitP)
  fromPair = Graph(\(PairP x y) -> return $ VecP $ fromPair (x, y))
  toPair = Graph(\(VecP v) -> return $ uncurry PairP $ toPair v)
