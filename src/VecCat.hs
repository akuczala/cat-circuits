{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
module VecCat(
    VecCat(..)
) where

import qualified Data.Vector.Sized as V
import GHC.TypeNats
import Data.Bifunctor (bimap)
import Graph
import Control.Category (Category (..), (>>>))
import Utils (postScanlVecM, splitScanlVecM)
import Data.Maybe (fromJust)
import Control.Arrow (Arrow(first, second))
import Data.Finite (Finite)

class Category k => VecCat k where
    indexVec :: Finite n -> V.Vector n a `k` a
    splitHead :: V.Vector (1 + n) a `k` (a, V.Vector n a)
    mergeHead :: (a, V.Vector n a) `k` V.Vector (1 + n) a
    flatOne :: V.Vector 1 a `k` a
    vanishNone :: V.Vector 0 a `k` ()
    fromPair :: (a, a) `k` V.Vector 2 a
    toPair :: V.Vector 2 a `k` (a, a)
    zipVecs :: (V.Vector n a, V.Vector n b) `k` V.Vector n (a, b)
    mapVec :: a `k` b -> V.Vector n a `k` V.Vector n b
    zipWithVec :: (a, b) `k` c -> (V.Vector n a, V.Vector n b) `k` V.Vector n c
    zipWithVec f = zipVecs >>> mapVec f
    foldlVec :: (b, a) `k` b -> (b, V.Vector n a) `k` b
    postScanlVec :: (b, a) `k` b -> (b, V.Vector n a) `k` V.Vector n b
    splitScanVec :: (b, a) `k` (b, c) -> (b, V.Vector n a) `k` (V.Vector n c, b)
    
instance VecCat (->) where
    indexVec = flip V.index
    splitHead :: V.Vector (1 + n) a -> (a, V.Vector n a)
    splitHead v = first V.head (V.splitAt v)
    mergeHead :: (a, V.Vector n a) -> V.Vector (1 + n) a
    mergeHead (x, xs) = V.cons x xs
    flatOne  = V.head
    vanishNone _ = ()
    fromPair = V.fromTuple
    toPair v = bimap flatOne flatOne (V.splitAt v)
    zipVecs = uncurry V.zip
    mapVec = V.map
    foldlVec f = uncurry $ foldl (curry f)
    postScanlVec f = uncurry $ V.postscanl (curry f)
    splitScanVec f (b0, as) = extract $ postScanlVec acc ((b0, Nothing), as) where
        acc ((b, _), a) = second Just $ f (b, a)
        extract bcv = (fmap (fromJust Prelude.. snd) bcv, V.last (V.cons b0 (fmap fst bcv)))
    

instance VecCat Graph where
    indexVec i = Graph(\(VecP v) -> return $ V.index v i )
    splitHead = Graph (\(VecP v) -> f v) where
        f v = return (PairP x (VecP y))
            where (x, y) = splitHead v
    mergeHead = Graph (\(PairP x (VecP xs)) -> return $ VecP (V.cons x xs))
    flatOne = Graph(\(VecP v) -> return $ flatOne v)
    vanishNone = Graph(\(VecP _) -> return UnitP)
    fromPair = Graph(\(PairP x y) -> return $ VecP $ fromPair (x, y))
    toPair = Graph(\(VecP v) -> return $ uncurry PairP $ toPair v)
    zipVecs = Graph(\(PairP (VecP v1) (VecP v2)) -> return $ VecP $ V.zipWith PairP v1 v2)
    mapVec (Graph f) = Graph(\(VecP v) -> go v) where
        go v = do
            u <- V.mapM f v
            return $ VecP u
    foldlVec (Graph f) = Graph(\(PairP pb (VecP v)) -> V.foldM (curry go) pb v) where
        go (b,a) = f (PairP b a)
    postScanlVec (Graph f) = Graph(\(PairP pb (VecP v)) -> VecP <$> postScanlVecM go pb v) where
        go b a = f (PairP b a)
    splitScanVec (Graph f) = Graph(\(PairP b0 (VecP as)) -> extract <$> splitScanlVecM go b0 as) where
        go b a = do
            x <- f (PairP b a)
            return $ case x of
                PairP y z -> (y, z)
        extract (cs, b) = PairP (VecP cs) b