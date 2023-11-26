module Utils(
    Pair,
    OnlyOne(..),
    addAnother,
    postScanlVecM
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Vector.Sized as V
import GHC.TypeNats
import Data.Maybe (fromJust)

type Pair a = (a, a)

data OnlyOne a = NothingYet | OnlyOne a | TooMany
    deriving Show
addAnother :: a -> OnlyOne a -> OnlyOne a
addAnother x o = case o of
    NothingYet -> OnlyOne x
    _ -> TooMany

-- Use NonEmpty because init list always has at least one element
scanTest :: (b -> a -> b) -> b -> [a] -> [b]
scanTest f b0 as = NE.toList $ foldl go (NE.singleton b0) as where
    go (b NE.:| bs) a = f b a NE.:| (b : bs)

postScan :: (b -> a -> b) -> b -> [a] -> [b]
postScan f b0 [] = []
postScan f b0 (a:as) = scanTest f (f b0 a) as


specialScan :: (b -> a -> (b, c)) -> b -> [a] -> [c]
specialScan f b0 [] = []
specialScan f b0 (a0 : as) = map snd $ scanTest acc (f b0 a0) as where
    acc (b, _) = f b


foldMTest :: Monad m => (b -> a -> m b) -> b -> [a] -> m b
foldMTest f b0 = foldl go (return b0) where
    go mb a = do
        b <- mb
        f b a

scanM :: Monad m => (b -> a -> m b) -> b -> [a] -> m [b]
scanM f b0 as = NE.toList <$> foldl go (return (b0 NE.:| [])) as where
    go mbs a = do
        b1 NE.:| bs <- mbs
        b2 <- f b1 a
        return $ b2 NE.:| b1 : bs

-- this version of scanM2 does too much work (it needs to sequence after doing a scan)
scanM2 :: Monad m => (b -> a -> m b) -> b -> [a] -> m [b]
scanM2 f b0 as = sequence $ scanl acc (return b0) as where
    acc mb a = do
        b <- mb
        f b a

-- postScanlVecM :: Monad m => (b -> a -> m b) -> b -> V.Vector n a -> m (V.Vector n b)
-- postScanlVecM f b0 as = sequence $ V.postscanl' acc (return b0) as where
--     acc mb a = do
--         b <- mb
--         f b a

-- the KnownNat constraint is needed only for the conversion to and from the list
postScanlVecM :: (KnownNat n, Monad m) => (b -> a -> m b) -> b -> V.Vector n a -> m (V.Vector n b)
postScanlVecM f b0 v = fromJust . V.fromList . reverse <$> postScanM f b0 (V.toList v)

postScanM :: Monad m => (b -> a -> m b) -> b -> [a] -> m [b]
postScanM f b0 [] = return []
postScanM f b0 (a:as) = do
    b1 <- f b0 a
    scanM f b1 as

specialScanM :: Monad m => (b -> a -> m (b, c)) -> b -> [a] -> m [(b, c)]
specialScanM f b0 [] = return []
specialScanM f b0 (a0 : as) = do
    t0 <- f b0 a0
    postScanM acc t0 as
    where
        acc (b, _) = f b

