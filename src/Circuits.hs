{-# LANGUAGE DataKinds #-}
module Circuits(
  boolVecToIntLilEnd,
  boolVecToIntBigEnd,
  intToBoolVecBigEnd,
  intToBoolVecLilEnd,
  halfAdder,
  fullAdder,
  twoBitAdder
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

boolVecToInt :: (KnownNat n) => (Int -> Int) -> V.Vector n Bool -> Int
boolVecToInt indexer bv = V.sum (V.zipWith term (V.map indexer $ V.enumFromN 0) bv) where
  term :: Int -> Bool -> Int
  term n b = fromEnum b * 2^n

-- convert a boolean vector to an integer, assuming little endian format
boolVecToIntLilEnd :: (KnownNat n) => V.Vector n Bool -> Int
boolVecToIntLilEnd = boolVecToInt id

-- convert a boolean vector to an integer, assuming big endian format
boolVecToIntBigEnd :: (KnownNat n) => V.Vector n Bool -> Int
boolVecToIntBigEnd bv = boolVecToInt (\i -> V.length bv - i - 1) bv

intToBoolVecLilEnd :: (KnownNat n) => Int -> V.Vector n Bool
intToBoolVecLilEnd i = V.map (testBit i) (V.enumFromN 0)

intToBoolVecBigEnd ::(KnownNat n) => Int -> V.Vector n Bool
intToBoolVecBigEnd i = V.reverse $ intToBoolVecLilEnd i

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
