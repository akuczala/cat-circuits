module GraphVizUtils(
    testGraph,
    testNode,
    serializeDotGraph
) where
--import Data.GraphViz.Attributes.HTML (Table (..), Attribute (..), Cell (LabelCell), Label (..), TextItem (..), Row(..))
import Data.GraphViz.Attributes.HTML
import Data.Text.Lazy(pack, unpack)
import Data.GraphViz.Attributes.Complete (PortName(..), Attribute (Label, Shape), Label (HtmlLabel), Shape (PlainText))
import Data.List (intersperse)
import Data.GraphViz (DotNode (DotNode, nodeID, nodeAttributes), DotGraph (..), DotStatements (..), PrintDot (toDot), DotEdge)
import Data.GraphViz.Printing (renderDot)


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

data PortData = PortData {portId :: String, portLabel :: String} deriving Show

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

data FancyNodeData = FancyNodeData {fancyNodeName :: String, fancyNodeInPorts :: [PortData], fancyNodeOutPorts :: [PortData]}
    deriving Show

fancyNodeTable :: FancyNodeData -> Table
fancyNodeTable nodeData = minimalTable rows where
    rows = [
        portsRow (fancyNodeInPorts nodeData),
        nodeRow (fancyNodeName nodeData),
        portsRow (fancyNodeOutPorts nodeData)
        ]

fancyNode :: String -> FancyNodeData -> DotNode String
fancyNode nodeId nodeData = DotNode {
    nodeID = nodeId,
    nodeAttributes = [
        Shape PlainText,
        Label (HtmlLabel $ Table $ fancyNodeTable nodeData)
    ]
}

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

testGraph :: DotNode n ->  DotGraph n
testGraph node = minimalDigraph [node] []

serializeDotGraph :: (PrintDot n) => DotGraph n -> String
serializeDotGraph g = unpack $ renderDot $ toDot g

testPort :: String -> PortData
testPort name = PortData {portId = name, portLabel = name}

testNode :: DotNode String
testNode = fancyNode "test" node where
    node = FancyNodeData {
        fancyNodeName = "testName",
        fancyNodeInPorts = [testPort "a", testPort "b", testPort "c"],
        fancyNodeOutPorts = [testPort "c", testPort "d"]
    }