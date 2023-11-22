module Examples(
  exampleOut
) where
import Control.Category.Cartesian
import CustomCats
import Graph
import Control.Category((>>>))
import qualified Control.Category as C
import Control.Category.Monoidal
import Circuits
import BoolCat (BoolCat(xorC))
import VecCat (VecCat(toPair, fromPair, splitHead))

test8 :: Graph () ()
test8 = copy
  >>> initialNode "cin" *** pairInput "x" "y"
  >>> fullAdder halfAdder
  >>> terminalNode "s" *** terminalNode "cout"
  >>> consume

test9 :: Graph () ()
test9 = copy
  >>> initialNode "cin" *** pairInput "x" "y"
  >>> fullAdder (genNode "HADD")
  >>> terminalNode "s" *** terminalNode "cout"
  >>> consume

test10 :: Graph () ()
test10 = pairInput "x" "y" >>> xorC >>> terminalNode "out"

test11 :: Graph () ()
test11 = initialNode "c_x_y"
  >>> splitHead
  >>> C.id *** toPair
  >>> fullAdder halfAdder
  >>> fromPair
  >>> terminalNode "s_c"

exampleOut :: [Node]
exampleOut = snd C.. snd $ runGraph test11 UnitP