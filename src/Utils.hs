{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Utils(
    Pair,
    OnlyOne(..),
    addAnother,
    postScanlVecM,
    splitScanlVecM
) where

import qualified Data.Vector.Sized as V
import Data.Maybe (fromJust)
import Control.Category.Monoidal (second')

type Pair a = (a, a)

data OnlyOne a = NothingYet | OnlyOne a | TooMany
    deriving Show
addAnother :: a -> OnlyOne a -> OnlyOne a
addAnother x o = case o of
    NothingYet -> OnlyOne x
    _ -> TooMany

postScanlVecM :: Monad m => (b -> a -> m b) -> b -> V.Vector n a -> m (V.Vector n b)
postScanlVecM f b0 as = snd <$> V.ifoldM acc t0 as where
    t0 = (b0, V.map (const b0) as) -- the second term gives us a dummy init vec
    acc (b, bv) i a = do
        bNext <- f b a
        return (bNext, bv V.// [(i, bNext)])
    
splitScanlVecM :: Monad m => (b -> a -> m (b, c)) -> b -> V.Vector n a -> m (V.Vector n c, b)
splitScanlVecM f b0 as  = extract <$> postScanlVecM acc (b0, Nothing) as where
    acc (b, _) a = fmap (second' Just) (f b a)
    extract bcv = (fmap (fromJust . snd) bcv, V.last (V.cons b0 (fmap fst bcv)))