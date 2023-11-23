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
    mergeHead :: (a, V.Vector n a) `k` V.Vector (1 + n) a
    flatOne :: V.Vector 1 a `k` a
    vanishNone :: V.Vector 0 a `k` ()
    fromPair :: (a, a) `k` V.Vector 2 a
    toPair :: V.Vector 2 a `k` (a, a)
    zipVecs :: (V.Vector n a, V.Vector n b) `k` V.Vector n (a, b)

instance VecCat (->) where
    splitHead v = first V.head (V.splitAt v)
    mergeHead :: (a, V.Vector n a) -> V.Vector (1 + n) a
    mergeHead (x, xs) = V.cons x xs
    flatOne  = V.head
    vanishNone _ = ()
    fromPair = V.fromTuple
    toPair v = bimap flatOne flatOne (V.splitAt v)
    zipVecs = uncurry V.zip

instance VecCat Graph where
    splitHead = Graph (\(VecP v) -> f v) where
        f v = return (PairP x (VecP y))
            where (x, y) = splitHead v
    mergeHead = Graph (\(PairP x (VecP xs)) -> return $ VecP (V.cons x xs))
    flatOne = Graph(\(VecP v) -> return $ flatOne v)
    vanishNone = Graph(\(VecP _) -> return UnitP)
    fromPair = Graph(\(PairP x y) -> return $ VecP $ fromPair (x, y))
    toPair = Graph(\(VecP v) -> return $ uncurry PairP $ toPair v)
    zipVecs = Graph(\(PairP (VecP v1) (VecP v2)) -> return $ VecP $ V.zipWith PairP v1 v2)
