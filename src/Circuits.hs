module Circuits(
  halfAdder,
  fullAdder
) where
import BoolCat
import Utils
import qualified Control.Category as C
import Control.Category.Monoidal
import Control.Category ((>>>))
import Control.Category.Cartesian

halfAdder :: BoolCat k => k (Pair Bool) (Pair Bool)
halfAdder = copy >>> xorC *** andC

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