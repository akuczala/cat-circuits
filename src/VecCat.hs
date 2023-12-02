{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
module VecCat(
    VecCat(..)
) where

import Prelude hiding ((.))
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
    init :: V.Vector (n + 1) a `k` V.Vector n a
    index :: Finite n -> V.Vector n a `k` a
    splitHead :: V.Vector (1 + n) a `k` (a, V.Vector n a)
    mergeHead :: (a, V.Vector n a) `k` V.Vector (1 + n) a
    flatOne :: V.Vector 1 a `k` a
    vanishNone :: V.Vector 0 a `k` ()
    fromPair :: (a, a) `k` V.Vector 2 a
    toPair :: V.Vector 2 a `k` (a, a)
    zip :: (V.Vector n a, V.Vector n b) `k` V.Vector n (a, b)
    map :: a `k` b -> V.Vector n a `k` V.Vector n b
    zipWith :: (a, b) `k` c -> (V.Vector n a, V.Vector n b) `k` V.Vector n c
    zipWith f = VecCat.zip >>> VecCat.map f
    foldl :: (b, a) `k` b -> (b, V.Vector n a) `k` b
    postScanl :: (b, a) `k` b -> (b, V.Vector n a) `k` V.Vector n b
    splitScan :: (b, a) `k` (b, c) -> (b, V.Vector n a) `k` (V.Vector n c, b)
    reverse :: V.Vector n a `k` V.Vector n a

instance VecCat (->) where
    init = V.init
    index = flip V.index
    splitHead :: V.Vector (1 + n) a -> (a, V.Vector n a)
    splitHead v = first V.head (V.splitAt v)
    mergeHead :: (a, V.Vector n a) -> V.Vector (1 + n) a
    mergeHead (x, xs) = V.cons x xs
    flatOne  = V.head
    vanishNone _ = ()
    fromPair = V.fromTuple
    toPair v = bimap flatOne flatOne (V.splitAt v)
    zip = uncurry V.zip
    map = V.map
    foldl f = uncurry $ V.foldl (curry f)
    postScanl f = uncurry $ V.postscanl (curry f)
    splitScan f (b0, as) = extract $ postScanl acc ((b0, Nothing), as) where
        acc ((b, _), a) = second Just $ f (b, a)
        extract bcv = (fmap (fromJust . snd) bcv, V.last (V.cons b0 (fmap fst bcv)))
    reverse = V.reverse


liftVec :: (V.Vector n (Ports a) -> Ports b) -> Graph (V.Vector n a) b
liftVec f = Graph $ \(VecP v) -> return (f v)

instance VecCat Graph where
    init = liftVec (VecP . V.init)
    index i = liftVec (`V.index` i)
    splitHead = liftVec f where
        f v = PairP x (VecP y)
            where (x, y) = splitHead v
    mergeHead = Graph (\(PairP x (VecP xs)) -> return $ VecP (V.cons x xs))
    flatOne = liftVec flatOne
    vanishNone :: Graph (V.Vector 0 a) ()
    vanishNone = liftVec (const UnitP)
    fromPair = Graph (\(PairP x y) -> return . VecP . fromPair $ (x, y))
    toPair = Graph (\(VecP v) -> return . uncurry PairP . toPair $ v)
    zip = Graph (\(PairP (VecP v1) (VecP v2)) -> return $ VecP $ V.zipWith PairP v1 v2)
    map (Graph f) = Graph (\(VecP v) -> go v) where
        go v = do
            u <- V.mapM f v
            return $ VecP u
    foldl (Graph f) = Graph (\(PairP pb (VecP v)) -> V.foldM (curry go) pb v) where
        go (b,a) = f (PairP b a)
    postScanl (Graph f) = Graph (\(PairP pb (VecP v)) -> VecP <$> postScanlVecM go pb v) where
        go b a = f (PairP b a)
    splitScan (Graph f) = Graph (\(PairP b0 (VecP as)) -> extract <$> splitScanlVecM go b0 as) where
        go b a = do
            x <- f (PairP b a)
            return $ case x of
                PairP y z -> (y, z)
        extract (cs, b) = PairP (VecP cs) b
    reverse = liftVec (VecP . VecCat.reverse)