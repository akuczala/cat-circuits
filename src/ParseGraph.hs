{-# LANGUAGE GADTs #-}
module ParseGraph (
    translateGraph,
    PortData(..),
    NodeData(..),
    NodePortData(..),
    nodeIdToString,
    nodeLabelToString,
    findPortNodes,
    PortNodes(..),
    NodeLabel(..)
    ) where
import Data.List (nub, nubBy)
import qualified Graph as G
import Utils
import Data.Maybe (mapMaybe)

data PortData = PortData {
    portId :: String,
    portLabel :: String,
    wire :: G.Port
    } deriving Show

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

parsePorts :: G.Ports a -> [G.Port]
parsePorts G.UnitP = []
parsePorts (G.BoolP p) = [p]
parsePorts (G.IntP p) = [p]
parsePorts (G.PairP ps1 ps2) = parsePorts ps1 ++ parsePorts ps2
parsePorts (G.VecP v) = concatMap parsePorts v

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
        _ -> map (\x -> "in" ++ show x) [0..]


nodeLabelToOutPortNames :: NodeLabel -> Int -> [String]
nodeLabelToOutPortNames l i = case l of
    HADDLabel -> ["s", "cout"]
    FADDLabel -> ["s", "cout"]
    _ -> case i of
        1 -> [" "]
        _ -> map (\x -> "out" ++ show x) [0..]

labelInPorts :: [String] -> [G.Port] -> [PortData]
labelInPorts = zipWith3 go [0..] where
    go i name p = PortData {
        wire = p,
        portId = getInPortId i,
        portLabel = name
        }

labelOutPorts :: [String] -> [G.Port] -> [PortData]
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

allPorts :: [NodeData] -> [G.Port]
allPorts nodes = nubBy (\p1 p2 -> G.portId p1 == G.portId p2) $ concatMap (map wire . inPorts) nodes

data NodePortData = NodePortData {nodePortNodeId :: NodeId, nodePortId :: String}
    deriving Show


data PortNodes = PortNodes {fromNodePort :: OnlyOne NodePortData, toNodePorts :: [NodePortData], wirePort :: G.Port}
    deriving Show

-- supports only ports with a single origin and multiple targets
findPortNodes :: G.Port -> [NodeData] -> PortNodes
findPortNodes port = foldl go PortNodes {fromNodePort=NothingYet, toNodePorts=[], wirePort=port} where
    go :: PortNodes -> NodeData -> PortNodes
    go pNodes node@(NodeData _ _ inPorts outPorts) = case (findMatchingPorts port (nodeId node) inPorts, findMatchingPorts port (nodeId node) outPorts) of
        ([], []) -> pNodes
        ([], [nodePort]) -> pNodes {fromNodePort = addAnother nodePort (fromNodePort pNodes)}
        (nodePorts, []) -> pNodes {toNodePorts = nodePorts ++ toNodePorts pNodes}
        _ -> error "Cannot have wires connected to both input and output ports of a node, for now."

findMatchingPorts :: G.Port -> NodeId -> [PortData] -> [NodePortData]
findMatchingPorts p nid  = mapMaybe go where
    go :: PortData -> Maybe NodePortData
    go pData = case (G.portId . wire) pData of
        w | w == G.portId p -> Just $ NodePortData {
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

translateGraph :: [G.Node] -> ([NodeData], [G.Port])
translateGraph inNodes = (nodes, ports) where
    nodes = zipWith parseNode [0..] inNodes
    ports = allPorts nodes