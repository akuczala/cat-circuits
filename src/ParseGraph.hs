{-# LANGUAGE GADTs #-}
module ParseGraph (translateGraph) where
import Conal
import Data.List (nub)

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

parseNode :: Int -> Node -> NodeData
parseNode i (Node name pIn pOut) = NodeData {
    nodeId = i,
    nodeLabel = name,
    inPorts = parsePorts pIn,
    outPorts = parsePorts pOut
    }

allPorts :: [NodeData] -> [Port]
allPorts nodes = nub $ concatMap inPorts nodes

data NodePair = NodePair {fromNode :: Maybe NodeData, toNode :: Maybe NodeData}
    deriving Show

findPortNodePair :: Port -> [NodeData] -> (Maybe NodeData, Maybe NodeData)
findPortNodePair port = foldl go (Nothing, Nothing) where
    go pair node@(NodeData _ _ inPorts outPorts) = case (outPorts, inPorts) of
        _ | port `elem` outPorts -> (Just node, snd pair)
        _ | port `elem` inPorts -> (fst pair, Just node)
        _ -> pair

nodeIdToString :: Int -> String 
nodeIdToString i = "n" ++ show i

nodeString :: NodeData -> String
nodeString node = (nodeIdToString . nodeId) node ++ " [label=" ++ nodeLabel node ++ "]"

serializePair :: (Maybe NodeData, Maybe NodeData) -> String
serializePair (Just n1, Just n2) = nodeIdToString (nodeId n1) ++ " -> " ++ nodeIdToString ( nodeId n2)
serializePair _ = "INVALID"

translateGraph :: [Node] -> [String]
translateGraph inNodes = nodeStrings ++ edgeStrings
    where
        nodes = zipWith parseNode [0..] inNodes
        ports = allPorts nodes
        nodeStrings = map nodeString nodes
        edgeStrings = map (serializePair . flip findPortNodePair nodes) ports

