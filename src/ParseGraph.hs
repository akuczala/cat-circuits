{-# LANGUAGE GADTs #-}
module ParseGraph (
    translateGraph,
    serializeGraph,
    PortData(..),
    NodeData(..),
    NodePortData(..),
    nodeIdToString,
    nodeLabelToString,
    findPortNodes,
    PortNodes(..),
    NodeLabel(..)
    ) where
import Data.List (nub, intercalate)
import Graph
import Utils
import Data.Maybe (mapMaybe)

data PortData = PortData {
    portId :: String,
    portLabel :: String,
    wireId :: Port
    } deriving Show

data NodeLabel = NotLabel | OrLabel | AndLabel | XorLabel | StringLabel String
    deriving Show

nodeLabelFromString :: String -> NodeLabel
nodeLabelFromString s = case s of
    "not" -> NotLabel
    "or" -> OrLabel
    "and" -> AndLabel
    "xor" -> XorLabel
    _ -> StringLabel s

nodeLabelToString :: NodeLabel -> String
nodeLabelToString l = case l of
    NotLabel -> "not"
    OrLabel -> "or"
    AndLabel -> "and"
    XorLabel -> "xor"
    StringLabel s -> s


data NodeData = NodeData {
    nodeId :: Int,
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

nodeLabelToInPortNames :: NodeLabel -> [String]
nodeLabelToInPortNames l = case l of
    NotLabel -> [" "]
    OrLabel -> ["x", "y"]
    AndLabel -> ["x", "y"]
    XorLabel -> ["x", "y"]
    StringLabel _ -> map (\x -> "in" ++ show x) [0..]


nodeLabelToOutPortNames :: NodeLabel -> [String]
nodeLabelToOutPortNames l = case l of
    NotLabel -> [" "]
    OrLabel -> [" "]
    AndLabel -> [" "]
    XorLabel -> [" "]
    StringLabel _ -> map (\x -> "out" ++ show x) [0..]

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

parseNode :: Int -> Node -> NodeData
parseNode i (Node name pIn pOut) = NodeData {
    nodeId = i,
    nodeLabel = label,
    inPorts = inPorts,
    outPorts = outPorts
    }
    where
        label = nodeLabelFromString name
        inPorts = labelInPorts (nodeLabelToInPortNames label) (parsePorts pIn)
        outPorts = labelOutPorts (nodeLabelToOutPortNames label) (parsePorts pOut)

allPorts :: [NodeData] -> [Port]
allPorts nodes = nub $ concatMap (map wireId . inPorts) nodes

data NodePortData = NodePortData {nodePortNode :: NodeData, nodePortId :: String}
    deriving Show


data PortNodes = PortNodes {fromNodePort :: OnlyOne NodePortData, toNodePorts :: [NodePortData]}
    deriving Show

-- supports only ports with a single origin and multiple targets
findPortNodes :: Port -> [NodeData] -> PortNodes
findPortNodes port = foldl go PortNodes {fromNodePort=NothingYet, toNodePorts=[]} where
    go :: PortNodes -> NodeData -> PortNodes
    go pNodes node@(NodeData _ _ inPorts outPorts) = case (findMatchingPorts port node inPorts, findMatchingPorts port node outPorts) of
        ([], []) -> pNodes
        ([], [nodePort]) -> pNodes {fromNodePort = addAnother nodePort (fromNodePort pNodes)}
        (nodePorts, []) -> pNodes {toNodePorts = nodePorts ++ toNodePorts pNodes}
        _ -> error "Cannot have wires connected to both input and output ports of a node, for now."


--  note: NodeData is redundant here but we can probably change PortNodes to accept a node id only rather than NodeData
findMatchingPorts :: Port -> NodeData -> [PortData] -> [NodePortData]
findMatchingPorts p npd  = mapMaybe go where
    go :: PortData -> Maybe NodePortData
    go pData = case wireId pData of
        w | w == p -> Just $ NodePortData {
            nodePortNode = npd,
            nodePortId = portId pData
        }
        _ -> Nothing

nodeLabelToShape :: NodeLabel -> String
nodeLabelToShape NotLabel= "invtriangle"
nodeLabelToShape OrLabel = "invhouse"
nodeLabelToShape AndLabel = "invtrapezium"
nodeLabelToShape _ = "ellipse"


nodeIdToString :: Int -> String
nodeIdToString i = "n" ++ show i

nodeString :: NodeData -> String
nodeString node = idStr ++ " [label=" ++ label ++ ", shape=" ++ shape ++ "]" where
    idStr = (nodeIdToString . nodeId) node
    label = nodeLabelToString $ nodeLabel node
    shape = nodeLabelToShape (nodeLabel node)

serializePortNodes :: PortNodes -> String
serializePortNodes pNodes = serializedInNodes ++ " -> " ++ serializedOutNodes where
    serializedInNodes = case fromNodePort pNodes of
        OnlyOne nodePort -> nodeIdToString (nodeId . nodePortNode $ nodePort)
        NothingYet -> "INVALID: Must have single origin node but got 0"
        TooMany -> "INVALID: Must have single origin node but got > 1"
    serializedOutNodes = "{" ++ intercalate "," (map (nodeIdToString . nodeId . nodePortNode) (toNodePorts pNodes)) ++ "}"

translateGraph :: [Node] -> ([NodeData], [Port])
translateGraph inNodes = (nodes, ports) where
    nodes = zipWith parseNode [0..] inNodes
    ports = allPorts nodes

serializeGraph :: [Node] -> [String]
serializeGraph inNodes = nodeStrings ++ edgeStrings
    where
        (nodes, ports) = translateGraph inNodes
        nodeStrings = map nodeString nodes
        edgeStrings = map (serializePortNodes . flip findPortNodes nodes) ports
