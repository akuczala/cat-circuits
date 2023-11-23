module GraphVizUtils(
    testGraph,
    testNode,
    serializeDotGraph,
    buildDotGraph
) where
--import Data.GraphViz.Attributes.HTML (Table (..), Attribute (..), Cell (LabelCell), Label (..), TextItem (..), Row(..))
import Data.GraphViz.Attributes.HTML
import Data.Text.Lazy(pack, unpack)
import Data.GraphViz.Attributes.Complete (PortName(..), Attribute (Label, Shape), Label (HtmlLabel), Shape (PlainText))
import Data.List (intersperse)
import Data.GraphViz (DotNode (DotNode, nodeID, nodeAttributes), DotGraph (..), DotStatements (..), PrintDot (toDot), DotEdge (DotEdge))
import Data.GraphViz.Printing (renderDot)

import ParseGraph(PortData(..), NodeData(..), nodeIdToString, nodeLabelToString, PortNodes(..), findPortNodes)
import Utils (OnlyOne(..))
import Graph (Port)

toTextLabel :: String -> Data.GraphViz.Attributes.HTML.Label
toTextLabel s = Text [Str $ pack s]

emptyCell :: Cell
emptyCell = LabelCell [Width 20] (toTextLabel "")

minimalTable :: [Row] -> Table
minimalTable rows = HTable {
        tableFontAttrs = Nothing,
        tableAttrs = [
        Border 0,
        CellBorder 0,
        CellSpacing 0,
        CellPadding 0
    ],
    tableRows = rows
    }

portCell :: PortData -> Cell
portCell portData = LabelCell attrs (toTextLabel $ portLabel portData) where
    attrs = [
        Port (PN (pack $ portId portData)),
        Border 1,
        CellPadding 1
        ]

portsRow :: [PortData] -> Row
portsRow ports = Cells [LabelCell [] (Table innerTable)] where
    innerTable = minimalTable [cells]
    cells = Cells $ [emptyCell] ++ intersperse emptyCell (map portCell ports) ++ [emptyCell]

nodeRow :: String -> Row
nodeRow name = Cells [cell] where
    cell = LabelCell [Border 1, CellPadding 4] (toTextLabel name)

data FancyNodeData = FancyNodeData {
    fancyNodeId :: String,
    fancyNodeName :: String,
    fancyNodeInPorts :: [PortData],
    fancyNodeOutPorts :: [PortData]
    }
    deriving Show

nodeDataToFancyNodeData :: NodeData -> FancyNodeData
nodeDataToFancyNodeData nodeData = FancyNodeData {
    fancyNodeId = nodeIdToString $ nodeId nodeData,
    fancyNodeName = nodeLabelToString $ nodeLabel nodeData,
    fancyNodeInPorts = inPorts nodeData,
    fancyNodeOutPorts = outPorts nodeData
}

fancyNodeTable :: FancyNodeData -> Table
fancyNodeTable nodeData = minimalTable rows where
    rows = [
        portsRow (fancyNodeInPorts nodeData),
        nodeRow (fancyNodeName nodeData),
        portsRow (fancyNodeOutPorts nodeData)
        ]

fancyNode :: FancyNodeData -> DotNode String
fancyNode nodeData = DotNode {
    nodeID = fancyNodeId nodeData,
    nodeAttributes = [
        Shape PlainText,
        Label (HtmlLabel $ Table $ fancyNodeTable nodeData)
    ]
}

-- TODO: include port information
portNodesToEdge :: PortNodes -> [DotEdge String]
portNodesToEdge pns = map makeEdge (toNodes pns) where
    makeEdge n = DotEdge fnid (nodeIdToString $ nodeId n) []
    fnid = case fromNode pns of
        OnlyOne fn -> nodeIdToString $ nodeId fn
        NothingYet -> "INVALID"
        TooMany -> "INVALID"

minimalDigraph :: [DotNode n] -> [DotEdge n] -> DotGraph n
minimalDigraph nodes edges = DotGraph {
    strictGraph = False,
    directedGraph = True,
    graphID = Nothing,
    graphStatements = DotStmts {
        attrStmts = [],
        subGraphs = [],
        nodeStmts = nodes,
        edgeStmts = edges
    }
}

buildDotGraph :: [NodeData] -> [Port] -> DotGraph String
buildDotGraph nodeDatas ports = minimalDigraph nodes edges where
    nodes = map (fancyNode . nodeDataToFancyNodeData) nodeDatas
    edges = concatMap (portNodesToEdge . flip findPortNodes nodeDatas) ports

testGraph :: DotNode n ->  DotGraph n
testGraph node = minimalDigraph [node] []

serializeDotGraph :: (PrintDot n) => DotGraph n -> String
serializeDotGraph g = unpack $ renderDot $ toDot g

testPort :: String -> PortData
testPort name = PortData {wireId = 0, portId = name, portLabel = name}

testNode :: DotNode String
testNode = fancyNode node where
    node = FancyNodeData {
        fancyNodeId = "nid",
        fancyNodeName = "testName",
        fancyNodeInPorts = [testPort "a", testPort "b", testPort "c"],
        fancyNodeOutPorts = [testPort "c", testPort "d"]
    }