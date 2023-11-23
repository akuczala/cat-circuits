module GraphVizUtils(
    testGraph,
    testNode,
    serializeDotGraph,
    buildDotGraph
) where
--import Data.GraphViz.Attributes.HTML (Table (..), Attribute (..), Cell (LabelCell), Label (..), TextItem (..), Row(..))
import Data.GraphViz.Attributes.HTML
import Data.Text.Lazy(pack, unpack)
import Data.GraphViz.Attributes.Complete (PortName(..), Attribute (Label, Shape), Label (HtmlLabel), Shape (..))
import Data.List (intersperse)
import Data.GraphViz (DotNode (DotNode, nodeID, nodeAttributes), DotGraph (..), DotStatements (..), PrintDot (toDot), DotEdge (DotEdge))
import Data.GraphViz.Printing (renderDot)

import ParseGraph(PortData(..), NodeData(..), nodeIdToString, nodeLabelToString, PortNodes(..), findPortNodes, NodeLabel(..), NodePortData(..))
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

nodeLabelToShape :: NodeLabel -> Shape
nodeLabelToShape NotLabel= InvTriangle
nodeLabelToShape OrLabel = InvHouse
nodeLabelToShape AndLabel = InvTrapezium
nodeLabelToShape _ = Ellipse

nodeRow :: FancyNodeData -> Row
nodeRow nodeData = Cells [cell] where
    cell = LabelCell [Border 1, CellPadding 4, Style Rounded] (toTextLabel . fancyNodeName $ nodeData )

data FancyNodeData = FancyNodeData {
    fancyNodeId :: String,
    fancyNodeName :: String,
    fancyNodeInPorts :: [PortData],
    fancyNodeOutPorts :: [PortData],
    fancyNodeShape :: Shape -- unused
    }
    deriving Show

nodeDataToFancyNodeData :: NodeData -> FancyNodeData
nodeDataToFancyNodeData nodeData = FancyNodeData {
    fancyNodeId = nodeIdToString $ nodeId nodeData,
    fancyNodeName = nodeLabelToString $ nodeLabel nodeData,
    fancyNodeInPorts = inPorts nodeData,
    fancyNodeOutPorts = outPorts nodeData,
    fancyNodeShape = nodeLabelToShape $ nodeLabel nodeData
}

fancyNodeTable :: FancyNodeData -> Table
fancyNodeTable nodeData = minimalTable rows where
    rows = [
        portsRow (fancyNodeInPorts nodeData),
        nodeRow nodeData,
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

nodePortString :: NodePortData -> String
nodePortString npd = nid ++ ":" ++ pid where
    nid = nodeIdToString . nodeId . nodePortNode $ npd
    pid = nodePortId npd

portNodesToEdge :: PortNodes -> [DotEdge String]
portNodesToEdge pns = map makeEdge (toNodePorts pns) where
    makeEdge n = DotEdge fnid (nodePortString n) []
    fnid = case fromNodePort pns of
        OnlyOne fn -> nodePortString fn
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

serializeDotGraph :: (PrintDot n) => DotGraph n -> String
serializeDotGraph g = unpack $ renderDot $ toDot g

testGraph :: DotNode n ->  DotGraph n
testGraph node = minimalDigraph [node] []

testPort :: String -> PortData
testPort name = PortData {wireId = 0, portId = name, portLabel = name}

testNode :: DotNode String
testNode = fancyNode node where
    node = FancyNodeData {
        fancyNodeId = "nid",
        fancyNodeName = "testName",
        fancyNodeInPorts = [testPort "a", testPort "b", testPort "c"],
        fancyNodeOutPorts = [testPort "c", testPort "d"],
        fancyNodeShape = Ellipse
    }