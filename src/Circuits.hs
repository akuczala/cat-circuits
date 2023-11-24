{-# LANGUAGE DataKinds #-}
module Circuits(
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