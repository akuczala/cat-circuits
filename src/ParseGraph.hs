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
import Data.List (nub)
import Graph
import Utils
import Data.Maybe (mapMaybe)

data PortData = PortData {
    portId :: String,
    portLabel :: String,
    wireId :: Port
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

parsePorts :: Ports a -> [Port]
parsePorts UnitP = []
parsePorts (BoolP p) = [p]
parsePorts (IntP p) = [p]
parsePorts (PairP ps1 ps2) = parsePorts ps1 ++ parsePorts ps2
parsePorts (VecP v) = concatMap parsePorts v

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

labelInPorts :: [String] -> [Port] -> [PortData]
labelInPorts = zipWith3 go [0..] where
    go i name p = PortData {
        wireId = p,
        portId = getInPortId i,
        portLabel = name
        }

labelOutPorts :: [String] -> [Port] -> [PortData]
labelOutPorts = zipWith3 go [0..] where
    go i name p = PortData {
        wireId = p,
        portId = getOutPortId i,
        portLabel = name
        }

parseNode :: NodeId -> Node -> NodeData
parseNode i (Node name pIn pOut) = NodeData {
    nodeId = i,
    nodeLabel = label,
    inPorts = labelInPorts (nodeLabelToInPortNames label $ length inPorts) inPorts,
    outPorts = labelOutPorts (nodeLabelToOutPortNames label $ length outPorts) outPorts
    }
    where
        label = nodeLabelFromString name
        inPorts =  parsePorts pIn
        outPorts = parsePorts pOut

allPorts :: [NodeData] -> [Port]
allPorts nodes = nub $ concatMap (map wireId . inPorts) nodes

data NodePortData = NodePortData {nodePortNodeId :: NodeId, nodePortId :: String}
    deriving Show


data PortNodes = PortNodes {fromNodePort :: OnlyOne NodePortData, toNodePorts :: [NodePortData]}
    deriving Show

-- supports only ports with a single origin and multiple targets
findPortNodes :: Port -> [NodeData] -> PortNodes
findPortNodes port = foldl go PortNodes {fromNodePort=NothingYet, toNodePorts=[]} where
    go :: PortNodes -> NodeData -> PortNodes
    go pNodes node@(NodeData _ _ inPorts outPorts) = case (findMatchingPorts port (nodeId node) inPorts, findMatchingPorts port (nodeId node) outPorts) of
        ([], []) -> pNodes
        ([], [nodePort]) -> pNodes {fromNodePort = addAnother nodePort (fromNodePort pNodes)}
        (nodePorts, []) -> pNodes {toNodePorts = nodePorts ++ toNodePorts pNodes}
        _ -> error "Cannot have wires connected to both input and output ports of a node, for now."


--  note: NodeData is redundant here but we can probably change PortNodes to accept a node id only rather than NodeData
findMatchingPorts :: Port -> NodeId -> [PortData] -> [NodePortData]
findMatchingPorts p nid  = mapMaybe go where
    go :: PortData -> Maybe NodePortData
    go pData = case wireId pData of
        w | w == p -> Just $ NodePortData {
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

translateGraph :: [Node] -> ([NodeData], [Port])
translateGraph inNodes = (nodes, ports) where
    nodes = zipWith parseNode [0..] inNodes
    ports = allPorts nodes