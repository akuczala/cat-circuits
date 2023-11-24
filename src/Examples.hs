{-# LANGUAGE DataKinds #-}
module Examples(
  writeDotExample,
  examples
) where
import Control.Category.Cartesian
import Graph
import Control.Category((>>>))
import qualified Control.Category as C
import Control.Category.Monoidal
import Circuits
import BoolCat (BoolCat(xorC))
import ParseGraph (translateGraph)
import GraphVizUtils (buildDotGraph, serializeDotGraph)
import Data.Function ((&))
import VecCat (VecCat(..))
import qualified Data.Vector.Sized as V

examples :: [Graph () ()]
examples = [
  pairInput "x" "y"
  >>> xorC
  >>> terminalNode "out",

  pairInput "x" "y"
  >>> halfAdder
  >>> terminalNode "sum" *** terminalNode "carry"
  >>> consume,

  copy
  >>> initialNode "cin" *** pairInput "x" "y"
  >>> fullAdder (genNode "HADD")
  >>> terminalNode "s" *** terminalNode "cout"
  >>> consume,

  copy
  >>> initialNode "cin" *** pairInput "x" "y"
  >>> fullAdder halfAdder
  >>> terminalNode "s" *** terminalNode "cout"
  >>> consume,

  copy
  >>> initialNode "cin" *** pairInput "x" "y"
  >>> twoBitAdder (genNode "FADD")
  >>> terminalNode "s" *** terminalNode "cout"
  >>> consume,

  copy
  >>> initialNode "cin" *** pairInput "x" "y"
  >>> twoBitAdder (fullAdder halfAdder)
  >>> terminalNode "s" *** terminalNode "cout"
  >>> consume,

  copy
  >>> inVec4 "x" *** inVec4 "y"
  >>> zipVecs
  >>> mapVec xorC
  >>> genNode "out",
  
  copy
  >>> initialNode "y" *** inVec4 "x"
  >>> foldlVec xorC
  >>> genNode "out"
  ]
 
inVec4 :: (GenPorts a) => String -> Graph () (V.Vector 4 a)
inVec4= genNode

exampleOut :: Graph () () -> [Node]
exampleOut graph = snd C.. snd $ runGraph graph UnitP

writeDotExample :: String -> Graph () () -> IO ()
writeDotExample fileName example = example & (
    exampleOut
    >>> translateGraph
    >>> uncurry buildDotGraph
    >>> serializeDotGraph
    >>> writeFile fileName
    )