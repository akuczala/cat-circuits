{-# LANGUAGE GADTs #-}
module ParseGraph (translateGraph, serializeGraph) where
import Data.List (nub, intercalate)
import Graph
import Utils

data NodeData = NodeData {
    nodeId :: Int,
    nodeLabel :: String,
    inPorts :: [Port],
    outPorts :: [Port]
    }
    deriving Show

parsePorts :: Ports a -> [Port]
parsePorts UnitP = []
parsePorts (BoolP p) = [p]
parsePorts (IntP p) = [p]
parsePorts (PairP ps1 ps2) = parsePorts ps1 ++ parsePorts ps2
parsePorts (VecP v) = concatMap parsePorts v

parseNode :: Int -> Node -> NodeData
parseNode i (Node name pIn pOut) = NodeData {
    nodeId = i,
    nodeLabel = name,
    inPorts = parsePorts pIn,
    outPorts = parsePorts pOut
    }

allPorts :: [NodeData] -> [Port]
allPorts nodes = nub $ concatMap inPorts nodes


data PortNodes = PortNodes {fromNode :: OnlyOne NodeData, toNodes :: [NodeData]}
    deriving Show

-- supports only ports with a single origin and multiple targets
findPortNodes :: Port -> [NodeData] -> PortNodes
findPortNodes port = foldl go PortNodes {fromNode=NothingYet, toNodes=[]} where
    go :: PortNodes -> NodeData -> PortNodes
    go pNodes node@(NodeData _ _ inPorts outPorts) = case (outPorts, inPorts) of
        _ | port `elem` outPorts -> pNodes{fromNode = addAnother node (fromNode pNodes)}
        _ | port `elem` inPorts -> pNodes {toNodes = node : toNodes pNodes}
        _ -> pNodes

nodeLabelToShape :: String -> String
nodeLabelToShape "not" = "invtriangle"
nodeLabelToShape "or" = "invhouse"
nodeLabelToShape "and" = "invtrapezium"
nodeLabelToShape _ = "ellipse"

nodeIdToString :: Int -> String
nodeIdToString i = "n" ++ show i

nodeString :: NodeData -> String
nodeString node = idStr ++ " [label=" ++ label ++ ", shape=" ++ shape ++ "]" where
    idStr = (nodeIdToString . nodeId) node
    label = nodeLabel node
    shape = nodeLabelToShape (nodeLabel node)

serializePortNodes :: PortNodes -> String
serializePortNodes pNodes = serializedInNodes ++ " -> " ++ serializedOutNodes where
    serializedInNodes = case fromNode pNodes of
        OnlyOne node -> nodeIdToString (nodeId node)
        NothingYet -> "INVALID: Must have single origin node but got 0"
        TooMany -> "INVALID: Must have single origin node but got > 1"
    serializedOutNodes = "{" ++ intercalate "," (map (nodeIdToString . nodeId) (toNodes pNodes)) ++ "}"

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
