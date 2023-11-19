module Examples where
import Control.Category.Cartesian
import CustomCats
import BoolCat
import Graph
import Utils
import Control.Category((>>>))
import qualified Control.Category as C
import Control.Category.Monoidal

test :: (Cartesian k) => k Bool (Bool, Bool)
test = copy >>> copy >>> fst'

test2 :: (Bimap k, BoolCat k) => k Bool (Bool, Bool)
test2 =
   C.id &&& notC

test3 ::(Bimap k, BoolCat k) => k (Bool, Bool) Bool
test3 =
  bimap C.id notC >>> andC

halfAdder :: BoolCat k => k (Pair Bool) (Pair Bool)
halfAdder = copy >>> xorC *** andC

halfAdderFork :: Graph (Pair Bool) (Pair (Pair Bool)) -> Graph (Pair Bool) (Pair Bool)
halfAdderFork fork = fork >>> bimap xorC andC


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

test5 :: Graph () ()
test5 = genNode "x" >>> notC >>> genNode "y"

test6 :: Graph () ()
test6 = copy >>> bimap (genNode "x") (genNode "y") >>> andC >>> genNode "a"

test7 :: Graph () ()
test7 = copy
  >>> bimap (genNode "x") (genNode "y")
  >>> copy >>> bimap xorC andC
  >>> bimap (terminalNode "s") (terminalNode "c")
  >>> consume

test8 :: Graph () ()
test8 = copy
  >>> initialNode "cin" *** pairInput "x" "y"
  >>> fullAdder (halfAdderFork copy)
  >>> terminalNode "s" *** terminalNode "cout"
  >>> consume

toGraph :: Graph a b -> Graph a b
toGraph = Prelude.id

toFun :: (a -> b) -> (a -> b)
toFun = Prelude.id

largePairPort :: Ports (Bool, Bool)
largePairPort = PairP (BoolP 100) (BoolP 101)

exampleOut :: [Node]
exampleOut = snd C.. snd $ runGraph test8 UnitP