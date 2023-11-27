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
  >>> initialNode "x" *** initialNode "y"
  >>> zipVecs
  >>> mapVec xorC
  >>> (genNode "out" :: TerminalNode (V.Vector 4 Bool)),
  
  copy
  >>> initialNode "x" *** (initialNode "y" :: InitialNode (V.Vector 4 Bool))
  >>> foldlVec xorC
  >>> genNode "out",

  copy
  >>> initialNode "x" *** initialNode "y"
  >>> postScanlVec xorC
  >>> (terminalNode "out"  :: TerminalNode (V.Vector 4 Bool)),

  copy
  >>> initialNode "cin" *** (copy >>> (genNode "x" *** genNode "y"))
  >>> nBitAdder (genNode "FADD")
  >>> (terminalNode "sum" :: TerminalNode (V.Vector 4 Bool)) *** terminalNode "cout"
  >>> consume
  ]

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