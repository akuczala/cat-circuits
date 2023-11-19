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

exampleOut :: [Node]
exampleOut = snd C.. snd $ runGraph test9 UnitP