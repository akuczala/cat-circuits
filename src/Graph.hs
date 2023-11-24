-- Much adapted from Conal Elliot's Compiling Categories paper
-- http://conal.net/papers/compiling-to-categories/
-- extended support to Chris Penner's catalyst typeclasses
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DataKinds #-}
module Graph(
  Port,
  Node(..),
  Graph(..),
  genNode,
  GenPorts,
  Ports(..),
  runGraph,
  GraphM,
  initialNode, terminalNode, pairInput
) where

import Control.Monad.State
import Control.Applicative (Applicative(liftA2))
import Control.Arrow (Arrow(second))

import Control.Category as C
import Control.Category.Cartesian (Cartesian(..))
import Control.Category.Monoidal (SymmetricProduct(..), MonoidalProduct(..))
import CustomCats (Bimap(..))
import qualified Data.Vector.Sized as V
import GHC.TypeNats


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
    VecP :: V.Vector n (Ports a) -> Ports (V.Vector n a)
    -- FunP :: Graph a b -> Ports (a -> b)

-- why does the below fail when I add the constraint Show a ?
instance Show (Ports a) where
  show UnitP = "UnitP"
  show (BoolP p) = "BoolP " ++ show p
  show (IntP p) = "IntP " ++ show p
  show (PairP ps1 ps2) = "PairP " ++ show ps1 ++ " " ++ show ps2
  show (VecP ps) = "VecP " ++ show (fmap show ps) -- TODO this likely has too many quotations

type NodeName = String

data Node = forall a b. Node NodeName (Ports a ) (Ports b) -- inputs, outputs
-- must use this instead of "deriving Show" for Reasons
deriving instance Show Node

instance Category Graph where
  id = Graph return
  Graph g . Graph f = Graph (g <=< f)

instance Cartesian Graph where
    fst' = Graph (\(PairP a _) -> return a)
    snd' = Graph (\(PairP _ b) -> return b)
    Graph f &&& Graph g = Graph (liftA2 (liftA2 PairP) f g)
    consume :: Graph a ()
    consume = Graph (\_ -> return UnitP)
    copy = C.id &&& C.id

instance Bimap Graph where
  bimap :: Graph a b -> Graph a c -> Graph (a, a) (b, c)
  bimap (Graph f) (Graph g) = Graph (
    \(PairP a b) -> do
      x <- f a
      y <- g b
      return (PairP x y)
    )

instance SymmetricProduct Graph where
  swap = Graph (\(PairP a b) -> return (PairP b a))
  reassoc = Graph (\(PairP a (PairP b c)) -> return (PairP (PairP a b) c))

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

forkNode :: (GenPorts a) => Graph a (a, a)
forkNode = genNode "fork"

terminalNode :: String -> Graph a ()
terminalNode = genNode

initialNode :: (GenPorts b) => String -> Graph () b
initialNode = genNode

pairInput :: (GenPorts a, GenPorts b) => String -> String -> Graph () (a, b)
pairInput x y = copy >>> genNode x *** genNode y

instance (KnownNat n, GenPorts a) => GenPorts (V.Vector n a) where
  genPorts = do
    xs <- V.replicateM genPorts
    return (VecP xs)