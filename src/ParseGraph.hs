{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
module ParseGraph (
    translateGraph,
    PortData(..),
    NodeData(..),
    NodePortData(..),
    AnyPort,
    getPortId,
    showPort,
    nodeIdToString,
    nodeLabelToString,
    findPortNodes,
    PortNodes(..),
    NodeLabel(..)
    ) where
import Data.List (nubBy)
import qualified Graph as G
import Utils
import Data.Maybe (mapMaybe)


data PortData = PortData {
    portId :: String,
    portLabel :: String,
    wire :: AnyPort
    }
deriving instance Show PortData

data NodeLabel = NotLabel | OrLabel | AndLabel | XorLabel | HADDLabel | FADDLabel | StringLabel String
    deriving Show

nodeLabelFromString :: String -> NodeLabel
nodeLabelFromString s = case s of
    "not" -> NotLabel
    "or" -> OrLabel
    "and" -> AndLabel
    "xor" -> XorLabel
    "FADD" -> FADDLabel
    "HADD" -> HADDLabel
    _ -> StringLabel s

nodeLabelToString :: NodeLabel -> String
nodeLabelToString l = case l of
    NotLabel -> "not"
    OrLabel -> "or"
    AndLabel -> "and"
    XorLabel -> "xor"
    FADDLabel -> "FADD"
    HADDLabel -> "HADD"
    StringLabel s -> s

type NodeId = Int

data NodeData = NodeData {
    nodeId :: NodeId,
    nodeLabel :: NodeLabel,
    inPorts :: [PortData],
    outPorts :: [PortData]
    }
    deriving Show

data AnyPort = forall a . Show a => AnyPort (G.Port a)
deriving instance Show AnyPort

getPortId :: AnyPort -> G.PortIndex
getPortId (AnyPort p)= G.portId p

showPort :: AnyPort -> Maybe String
showPort (AnyPort a) = fmap show (G.portValue a)

parsePorts :: G.Ports a -> [AnyPort]
parsePorts G.UnitP = []
parsePorts (G.BoolP p) = [AnyPort p]
parsePorts (G.IntP p) = [AnyPort p]
parsePorts (G.PairP ps1 ps2) = parsePorts ps1 ++ parsePorts ps2
parsePorts (G.VecP v) = concatMap parsePorts v
parsePorts (G.FunP _) = error "parsing function ports unsupported"

getInPortId :: Int -> String
getInPortId i = "in" ++ show i

getOutPortId :: Int -> String
getOutPortId i = "out" ++ show i

nodeLabelToInPortNames :: NodeLabel -> Int -> [String] -- int is len of ports
nodeLabelToInPortNames l n = case l of
    FADDLabel -> ["cin", "x", "y"]
    _ -> case n of
        1 -> [" "]
        2 -> ["x", "y"]
        _ -> map (\x -> "in" ++ show x) ([0..] :: [Integer])


nodeLabelToOutPortNames :: NodeLabel -> Int -> [String]
nodeLabelToOutPortNames l i = case l of
    HADDLabel -> ["s", "cout"]
    FADDLabel -> ["s", "cout"]
    _ -> case i of
        1 -> [" "]
        _ -> map (\x -> "out" ++ show x) ([0..] :: [Integer])

labelInPorts :: [String] -> [AnyPort] -> [PortData]
labelInPorts = zipWith3 go [0..] where
    go i name p = PortData {
        wire = p,
        portId = getInPortId i,
        portLabel = name
        }

labelOutPorts :: [String] -> [AnyPort] -> [PortData]
labelOutPorts = zipWith3 go [0..] where
    go i name p = PortData {
        wire = p,
        portId = getOutPortId i,
        portLabel = name
        }

parseNode :: NodeId -> G.Node -> NodeData
parseNode i (G.Node name pIn pOut) = NodeData {
    nodeId = i,
    nodeLabel = label,
    inPorts = labelInPorts (nodeLabelToInPortNames label $ length inPorts) inPorts,
    outPorts = labelOutPorts (nodeLabelToOutPortNames label $ length outPorts) outPorts
    }
    where
        label = nodeLabelFromString name
        inPorts =  parsePorts pIn
        outPorts = parsePorts pOut

allPorts :: [NodeData] -> [AnyPort]
allPorts nodes = nubBy (\p1 p2 -> getPortId p1 == getPortId p2) $ concatMap (map wire . inPorts) nodes

data NodePortData = NodePortData {nodePortNodeId :: NodeId, nodePortId :: String}
    deriving Show


data PortNodes = PortNodes {fromNodePort :: OnlyOne NodePortData, toNodePorts :: [NodePortData], wirePort :: AnyPort}
deriving instance Show PortNodes

-- supports only ports with a single origin and multiple targets
findPortNodes :: AnyPort -> [NodeData] -> PortNodes
findPortNodes port = foldl go PortNodes {fromNodePort=NothingYet, toNodePorts=[], wirePort=port} where
    go :: PortNodes -> NodeData -> PortNodes
    go pNodes node@(NodeData _ _ inPorts outPorts) = case (findMatchingPorts port (nodeId node) inPorts, findMatchingPorts port (nodeId node) outPorts) of
        ([], []) -> pNodes
        ([], [nodePort]) -> pNodes {fromNodePort = addAnother nodePort (fromNodePort pNodes)}
        (nodePorts, []) -> pNodes {toNodePorts = nodePorts ++ toNodePorts pNodes}
        _ -> error "Cannot have wires connected to both input and output ports of a node, for now."

findMatchingPorts :: AnyPort -> NodeId -> [PortData] -> [NodePortData]
findMatchingPorts p nid  = mapMaybe go where
    go :: PortData -> Maybe NodePortData
    go pData = case (getPortId . wire) pData of
        w | w == getPortId p -> Just $ NodePortData {
            nodePortNodeId = nid,
            nodePortId = portId pData
        }
        _ -> Nothing

nodeLabelToShape :: NodeLabel -> String
nodeLabelToShape NotLabel= "invtriangle"
nodeLabelToShape OrLabel = "invhouse"
nodeLabelToShape AndLabel = "invtrapezium"
nodeLabelToShape _ = "ellipse"


nodeIdToString :: NodeId -> String
nodeIdToString i = "n" ++ show i

translateGraph :: [G.Node] -> ([NodeData], [AnyPort])
translateGraph inNodes = (nodes, ports) where
    nodes = zipWith parseNode [0..] inNodes
    ports = allPorts nodes