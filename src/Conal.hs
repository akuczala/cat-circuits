{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE InstanceSigs #-}
module Conal where
-- import Control.Category (Category (id, (.)))
import Control.Monad.State
import Control.Applicative (Applicative(liftA2))
import Control.Arrow (Arrow(second), (>>>))
import ConalCats
import Control.Category(Category(..))
import Control.Category as C
-- import Control.Category.Cartesian (Cartesian (fst'))



newtype Graph a b = Graph(Ports a -> GraphM (Ports b))

initState :: (Port, [Node])
initState = (0, [])
runGraph :: Graph a b -> Ports a -> (Ports b, (Port, [Node]))
runGraph (Graph f) ports = runState (f ports) initState

type GraphM = State (Port, [Node])
type Port = Int
data Ports a where
    UnitP :: Ports ()
    BoolP :: Port -> Ports Bool
    IntP :: Port -> Ports Int
    PairP :: Ports a -> Ports b -> Ports (a, b)
    -- FunP :: Graph a b -> Ports (a -> b)

-- instance Show a => Show (Ports a) where
--   show x = case x of
--     UnitP -> "UnitP"
--     (BoolP p) -> "BoolP " ++ show p
--     (IntP p) -> "IntP " ++ show p
    --(PairP ps1 ps2) -> "PairP "
    
-- why does the below fail when I add the constraint Show a ?
instance Show (Ports a) where
  show UnitP = "UnitP"
  show (BoolP p) = "BoolP " ++ show p
  show (IntP p) = "IntP " ++ show p
  show (PairP ps1 ps2) = "PairP " ++ show ps1 ++ " " ++ show ps2



-- instance Show (Ports ()) where
--   show _ = "UnitP"

-- instance (Show (Ports a), Show (Ports b)) => Show (Ports (a, b)) where
--   show (PairP ps1 ps2) = "PairP " ++ show ps1 ++ " " ++ show ps2

type NodeName = String

data Node = forall a b. Node NodeName (Ports a ) (Ports b) -- inputs, outputs
-- must use this instead of "deriving Show" for Reasons
deriving instance Show Node

-- newtype State s a = State {runState :: s -> (a, s)}
-- instance Monad (State s) where

instance Category Graph where
  id = Graph return
  Graph g . Graph f = Graph (g <=< f)

instance Cartesian Graph where
    fst' = Graph (\(PairP a _) -> return a)
    snd' = Graph (\(PairP _ b) -> return b)
    Graph f &&& Graph g = Graph (liftA2 (liftA2 PairP) f g)
    consume :: Graph a ()
    consume = Graph (\_ -> return UnitP)

instance Bimap Graph where
  bimap :: Graph a b -> Graph a c -> Graph (a, a) (b, c)
  bimap (Graph f) (Graph g) = Graph(
    \(PairP a b) -> do
      x <- f a
      y <- g b
      return (PairP x y)
    )

instance SymmetricProduct Graph where
  swap = Graph(\(PairP a b) -> return (PairP b a))
  reassoc = Graph(\(PairP a (PairP b c)) -> return (PairP (PairP a b) c))

instance MonoidalProduct Graph where
  first' (Graph f) = Graph (
    \(PairP a b) -> do
      x <- f a
      return (PairP x b)
   )
  second' (Graph f) = Graph (
    \(PairP a b) -> do
      y <- f b
      return (PairP a y)
   )
  

genPort :: GraphM Port
genPort = do
    (o, nodes) <- get
    put (o + 1, nodes)
    return o

class GenPorts a where
    genPorts :: GraphM (Ports a)

instance GenPorts () where
  genPorts = return UnitP

instance GenPorts Bool where
  genPorts = fmap BoolP genPort

instance GenPorts Int where
  genPorts = fmap IntP genPort

instance (GenPorts a, GenPorts b) => GenPorts (a, b) where
  genPorts = liftA2 PairP genPorts genPorts

genNode :: GenPorts b => String -> Graph a b
genNode name = Graph (
  \a -> do
    b <- genPorts
    modify (second (Node name a b :))
    return b
  )

class Cartesian k => BoolCat k where
  notC :: k Bool Bool
  andC, orC, xorC :: k (Bool, Bool) Bool

class NumCat k a where
  negateC :: k a a
  addC, mulC :: k (a, a) a

instance BoolCat (->) where
  notC = not
  andC = and
  orC = or
  xorC (x, y) = x /= y

instance Num a => NumCat (->) a where
  negateC = negate
  addC = uncurry (+)
  mulC = uncurry (*)

instance BoolCat Graph where
  notC = genNode "not"
  andC = genNode "and"
  orC = genNode "or"
  xorC = genNode "xor"

class InitialCat k a where
  initial :: k () a

instance GenPorts a => InitialCat Graph a where
  initial = genNode "initial"

terminalNode :: String -> Graph a ()
terminalNode = genNode

initialNode :: (GenPorts b) => String -> Graph () b
initialNode = genNode

type Dup a = (a, a)

forkNode :: (GenPorts a) => Graph a (a, a)
forkNode = genNode "fork"

-- part of Control.Monad.State.Lazy:
-- modify :: MonadState s m => (s -> s) -> m ()
-- maps an old state to a new state, throwing away old state
-- nothing is returned, hence return type m ()

test :: (Cartesian k) => k Bool (Bool, Bool)
test = copy >>> copy >>> fst'

test2 :: (Bimap k, BoolCat k) => k Bool (Bool, Bool)
test2 =
   C.id &&& notC

test3 ::(Bimap k, BoolCat k) => k (Bool, Bool) Bool
test3 =
  bimap C.id notC >>> andC

halfAdder :: (Bimap k, BoolCat k) => k (Bool, Bool) (Bool, Bool)
halfAdder = copy >>> bimap xorC andC

halfAdderFork :: Graph (Dup Bool) (Dup (Dup Bool)) -> Graph (Bool, Bool) (Bool, Bool)
halfAdderFork fork = fork >>> bimap xorC andC


fullAdder
  :: (MonoidalProduct k, BoolCat k)
  => k (Bool, Bool) (Bool, Bool)
  -> k (Bool, (Bool, Bool)) (Bool, Bool)
fullAdder halfAdd =
  C.id *** halfAdd
  >>> reassoc
  >>> halfAdd *** C.id
  >>> swap >>> reassoc >>> swap
  >>> C.id *** orC

test4 :: (Bimap k, BoolCat k, InitialCat k Bool) => k () ()
test4 = initial >>> notC >>> consume

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
  >>> C.id *** copy
  >>> initialNode "cin" *** (initialNode "a" *** initialNode "b")
  >>> fullAdder (halfAdderFork forkNode)
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