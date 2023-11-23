{-# LANGUAGE DataKinds #-}
module Circuits(
  halfAdder,
  fullAdder,
  twoBitAdder
) where
import BoolCat
import Utils
import qualified Control.Category as C
import Control.Category.Monoidal
    ( MonoidalProduct((***)), SymmetricProduct(swap, reassoc) )
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
  C.id *** halfAdd
  >>> reassoc
  >>> halfAdd *** C.id
  >>> swap >>> reassoc >>> swap
  >>> C.id *** orC

-- 2 bit adder
-- (ci, (vec 2, vec2)) -> (co, vec2)
-- (ci, ((x0, x1), (y0, y1)))
-- (ci, ((x0, y0), (x1, y1)))
-- ((ci, (x0, y0)), (x1, y1))
-- ((c0, s0), (x1, y1))

-- todo: fix the last part of this
twoBitAdder
  :: (BoolCat k, VecCat k)
  => k (Bool, Pair Bool) (Pair Bool)
  -> k (Bool, (V.Vector 2 Bool, V.Vector 2 Bool)) (Bool, V.Vector 2 Bool)
twoBitAdder fullAdd =
  C.id *** (zipVecs >>> toPair)
  >>> reassoc
  >>> fullAdd *** C.id
  >>> swap >>> reassoc >>> swap
  >>> C.id *** (swap >>> reassoc >>> swap)
  >>> C.id *** fullAdd
  >>> C.id *** swap
  >>> reassoc
  >>> swap *** C.id
  >>> (swap >>> reassoc >>> swap)
  >>> C.id *** fromPair
