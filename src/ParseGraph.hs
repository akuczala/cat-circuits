{-# LANGUAGE GADTs #-}
module ParseGraph (translateGraph) where
import Conal

data NodeData = NodeData {name :: String, inPorts :: [Port], outPorts :: [Port]}
    deriving Show

parsePorts :: Ports a -> [Port]
parsePorts UnitP = []
parsePorts (BoolP p) = [p]
parsePorts (IntP p) = [p]
parsePorts (PairP ps1 ps2) = parsePorts ps1 ++ parsePorts ps2

parseNode :: Int -> Node -> NodeData
parseNode i (Node name pIn pOut) = NodeData {
    name = name ++ "_" ++ show i,
    inPorts = parsePorts pIn,
    outPorts = parsePorts pOut
    }

allPorts :: [NodeData] -> [Port]
allPorts = concatMap inPorts -- assume all edges are connected to one in and one out port

data NodePair = NodePair {fromNode :: Maybe NodeData, toNode :: Maybe NodeData}
    deriving Show

findPortNodePair :: Port -> [NodeData] -> (Maybe NodeData, Maybe NodeData)
findPortNodePair port = foldl go (Nothing, Nothing) where
    go pair node@(NodeData _ inPorts outPorts) = case (outPorts, inPorts) of
        _ | port `elem` outPorts -> (Just node, snd pair)
        _ | port `elem` inPorts -> (fst pair, Just node)
        _ -> pair

serializePair :: (Maybe NodeData, Maybe NodeData) -> String
serializePair (Just n1, Just n2) = name n1 ++ " -> " ++ name n2
serializePair _ = "INVALID"

translateGraph :: [Node] -> [String]
translateGraph inNodes = map (serializePair . flip findPortNodePair nodes) ports
    where
        nodes = zipWith parseNode [0..] inNodes
        ports = allPorts nodes

