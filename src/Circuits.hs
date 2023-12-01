{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Circuits(
  BoolVecCat,
  boolVecToIntLilEnd,
  boolVecToIntBigEnd,
  intToBoolVecBigEnd,
  intToBoolVecLilEnd,
  halfAdder,
  fullAdder,
  twoBitAdder,
  nBitAdder
) where
import BoolCat
import Utils
import Control.Category.Monoidal
    ( MonoidalProduct(..), SymmetricProduct(swap, reassoc))
import Control.Category ((>>>))
import Control.Category.Cartesian
import VecCat (VecCat (..))
import qualified Data.Vector.Sized as V
import GHC.TypeNats
import Data.Bits (testBit)
import Graph (Graph(..), genNodeFn)

class (BoolCat k, VecCat k) => BoolVecCat k where
  -- convert a boolean vector to an integer, assuming little / big endian format
  boolVecToIntLilEnd :: (KnownNat n) => V.Vector n Bool `k` Int
  boolVecToIntBigEnd :: (KnownNat n) => V.Vector n Bool `k` Int

  -- convert an integer to a boolean vector, assuming little / big endian format
  intToBoolVecLilEnd :: (KnownNat n) => Int `k` V.Vector n Bool
  intToBoolVecBigEnd ::(KnownNat n) => Int `k` V.Vector n Bool

boolVecToInt :: (KnownNat n) => (Int -> Int) -> V.Vector n Bool -> Int
boolVecToInt indexer bv = V.sum (V.zipWith term (V.map indexer $ V.enumFromN 0) bv) where
  term :: Int -> Bool -> Int
  term n b = fromEnum b * 2^n

instance BoolVecCat (->) where
  boolVecToIntLilEnd = boolVecToInt id
  boolVecToIntBigEnd bv = boolVecToInt (\i -> V.length bv - i - 1) bv
  intToBoolVecLilEnd i = V.map (testBit i) (V.enumFromN 0)
  intToBoolVecBigEnd i = VecCat.reverse $ intToBoolVecLilEnd i

instance BoolVecCat Graph where
  boolVecToIntLilEnd = genNodeFn "lilEnd" boolVecToIntLilEnd
  boolVecToIntBigEnd = genNodeFn "lilEnd" boolVecToIntBigEnd
  intToBoolVecLilEnd = genNodeFn "lilEnd" intToBoolVecLilEnd
  intToBoolVecBigEnd = genNodeFn "bigEnd" intToBoolVecBigEnd

-- (x, y) `k` (sum, carry)
halfAdder :: BoolCat k => k (Pair Bool) (Pair Bool)
halfAdder = copy >>> xorC *** andC

-- (cin, (x, y)) `k` (sum, cout)
fullAdder
  :: (BoolCat k)
  => k (Pair Bool) (Pair Bool)
  -> k (Bool, Pair Bool) (Pair Bool)
fullAdder halfAdd =
  second' halfAdd
  >>> reassoc
  >>> first' (halfAdd >>> swap)
  >>> swap >>> reassoc >>> swap
  >>> second' orC

-- implementation "by hand" gets real clunky fast
twoBitAdder
  :: (BoolCat k, VecCat k)
  => k (Bool, Pair Bool) (Pair Bool)
  -> k (Bool, (V.Vector 2 Bool, V.Vector 2 Bool)) (V.Vector 2 Bool, Bool)
twoBitAdder fullAdd =
  second' (zipVecs >>> toPair)
  >>> reassoc
  >>> first' (fullAdd >>> swap)
  >>> swap >>> reassoc >>> swap
  >>> second' (swap >>> fullAdd)
  >>> reassoc
  >>> first' fromPair

-- (cin, (x, y)) `k` (sum, cout)
nBitAdder
  :: (BoolCat k, VecCat k)
  => k (Bool, Pair Bool) (Pair Bool)
  -> k (Bool, (V.Vector n Bool, V.Vector n Bool)) (V.Vector n Bool, Bool)
nBitAdder fullAdd
  = second' zipVecs
  >>> splitScanVec (fullAdd >>> swap)