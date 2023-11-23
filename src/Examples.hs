module Examples(
  exampleOut,
  test8, test9, test10, test11, test12
) where
import Control.Category.Cartesian
import Graph
import Control.Category((>>>))
import qualified Control.Category as C
import Control.Category.Monoidal
import Circuits
import BoolCat (BoolCat(xorC))
import VecCat (VecCat(toPair, fromPair, splitHead))
import Utils

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
  >>> labeledFullAdder (fullAdder (genNode "HADD"))
  >>> fromPair
  >>> terminalNode "s_c"

labeledFullAdder
  :: Graph (Bool, Pair Bool) (Pair Bool)
  -> Graph (Bool, Pair Bool) (Pair Bool)
labeledFullAdder fullAdd =
  genNode "cin" *** (genNode "x" *** genNode "y")
  >>> fullAdd
  >>> (genNode "s" *** genNode "cout") 

test12 :: Graph () ()
test12 =
  copy
  >>> initialNode "cin" *** pairInput "x" "y"
  >>> twoBitAdder (labeledFullAdder $ genNode "FADD")
  >>> terminalNode "cout" *** terminalNode "s"
  >>> consume


exampleOut :: Graph () () -> [Node]
exampleOut graph = snd C.. snd $ runGraph graph UnitP