-- Much adapted from Conal Elliot's Compiling Categories paper
-- http://conal.net/papers/compiling-to-categories/
-- extended support to Christ Penner's catalyst typeclasses
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE InstanceSigs #-}
module Graph where

import Control.Monad.State
import Control.Applicative (Applicative(liftA2))
import Control.Arrow (Arrow(second))

import Control.Category as C
import Control.Category.Cartesian (Cartesian(..))
import Control.Category.Monoidal (SymmetricProduct(..), MonoidalProduct(..))
import CustomCats (Bimap(..))


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

-- why does the below fail when I add the constraint Show a ?
instance Show (Ports a) where
  show UnitP = "UnitP"
  show (BoolP p) = "BoolP " ++ show p
  show (IntP p) = "IntP " ++ show p
  show (PairP ps1 ps2) = "PairP " ++ show ps1 ++ " " ++ show ps2

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


-- part of Control.Monad.State.Lazy:
-- modify :: MonadState s m => (s -> s) -> m ()
-- maps an old state to a new state, throwing away old state
-- nothing is returned, hence return type m ()
