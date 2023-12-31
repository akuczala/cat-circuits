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
{-# LANGUAGE KindSignatures #-}
module Graph(
  Port(..),
  portPairToTuple,
  Node(..),
  Graph(..),
  PortIndex,
  genNode,
  genNodeFn,
  GenPorts,
  Ports(..),
  runGraph,
  GraphM,
  InitialNode,
  TerminalNode,
  initialNode, terminalNode, pairInput
) where

import Control.Monad.State
    ( State, (<=<), modify, runState, MonadState(put, get) )
import Control.Applicative (Applicative(liftA2))
import Control.Arrow (Arrow(second))

import Control.Category as C
import qualified CustomCats as Cu
import Control.Category.Cartesian (Cartesian(..))
import Control.Category.Monoidal (SymmetricProduct(..), MonoidalProduct(..))
import CustomCats (Bimap(..))
import qualified Data.Vector.Sized as V
import GHC.TypeNats
import GHC.Base (Type)


newtype Graph a b = Graph (Ports a -> GraphM (Ports b))

initState :: (PortIndex, [Node])
initState = (0, [])
runGraph :: Graph a b -> Ports a -> (Ports b, (PortIndex, [Node]))
runGraph (Graph f) ports = runState (f ports) initState

type PortIndex = Int
type GraphM = State (PortIndex, [Node])

data Port a = Port { portId :: PortIndex, portValue :: Maybe a} deriving Show

data Ports :: Type -> Type where
    UnitP :: Ports ()
    BoolP :: Port Bool -> Ports Bool
    IntP :: Port Int -> Ports Int
    -- FiniteP :: Proxy n -> Port -> Ports (Finite n)
    PairP :: Ports a -> Ports b -> Ports (a, b)
    VecP :: V.Vector n (Ports a) -> Ports (V.Vector n a)
    FunP :: Graph a b -> Ports (a -> b)

-- why does the below fail when I add the constraint Show a ?
instance Show (Ports a) where
  show UnitP = "UnitP"
  show (BoolP p) = "BoolP " ++ show p
  show (IntP p) = "IntP" ++ show p
  -- show (FiniteP _ p) = "FiniteP" ++ show p
  show (PairP ps1 ps2) = "PairP ( " ++ show ps1 ++ ", " ++ show ps2 ++ " )"
  show (VecP ps) = "VecP " ++ show (fmap show ps) -- TODO this likely has too many quotations
  show (FunP _) = "FunP"

portPairToTuple :: Ports (a, b) -> (Ports a, Ports b)
portPairToTuple (PairP a b) = (a, b)

unpackPortValues :: Ports a -> Maybe a
unpackPortValues ps = case ps of
  UnitP -> Just ()
  BoolP p -> portValue p
  IntP p -> portValue p
  --FiniteP _ p -> fmap (\(FinitePortValue i) -> i) (portValue p)
  PairP p1 p2 -> do
    v1 <- unpackPortValues p1
    v2 <- unpackPortValues p2
    return (v1, v2)
  VecP v -> V.mapM unpackPortValues v
  FunP (Graph _) -> Nothing


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

instance Cu.Closed Graph where
  apply = Graph(\(PairP (FunP (Graph f)) a) -> f a)
  curry (Graph f) = Graph (\a -> return $ FunP $ Graph $ \b -> f (PairP a b))
  uncurry (Graph g) = Graph (
    \(PairP a b) -> x a b
    ) where
      x a b = do
        y <- g a
        case y of -- need case statement to make haskell happy
          FunP (Graph f) -> f b

instance Cu.IntCat Graph where
  const i = genNodeFn (show i) (const i)

genPort :: Maybe a ->  GraphM (Port a)
genPort newValue = do
    (o, nodes) <- get
    put (o + 1, nodes)
    return $ Port o newValue

class GenPorts a where
    genPorts :: Maybe a -> GraphM (Ports a)

instance GenPorts () where
  genPorts _ = return UnitP

instance GenPorts Bool where
  genPorts b = fmap BoolP (genPort b)

instance GenPorts Int where
  genPorts i = fmap IntP (genPort i)

instance (GenPorts a, GenPorts b) => GenPorts (a, b) where
  genPorts pair = liftA2 PairP (genPorts $ fmap fst pair) (genPorts $ fmap snd pair)

instance (KnownNat n, GenPorts a) => GenPorts (V.Vector n a) where
  genPorts mv = do
    xs <- case mv of
      Just v -> V.mapM (genPorts Prelude.. Just) v
      Nothing -> V.replicateM (genPorts Nothing)
    return (VecP xs)

genNode :: GenPorts b => String -> Graph a b
genNode name = Graph (
  \a -> do
    b <- genPorts Nothing
    modify (second (Node name a b :))
    return b
  )

genNodeFn :: GenPorts b => String -> (a -> b) -> Graph a b
genNodeFn name f = Graph (
  \a -> do
    b <- genPorts $ fmap f (unpackPortValues a)
    modify (second (Node name a b :))
    return b
  )

-- nameToFn :: String -> 

type InitialNode a = Graph () a
type TerminalNode a = Graph a ()

terminalNode :: String -> TerminalNode a
terminalNode = genNode

initialNode :: (GenPorts b) => String -> InitialNode b
initialNode = genNode

pairInput :: (GenPorts a, GenPorts b) => String -> String -> Graph () (a, b)
pairInput x y = copy >>> genNode x *** genNode y
