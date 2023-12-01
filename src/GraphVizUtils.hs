module GraphVizUtils(
    serializeDotGraph,
    buildDotGraph
) where

import Data.GraphViz.Attributes.HTML
import Data.Text.Lazy(pack, unpack)
import Data.GraphViz.Attributes.Complete (PortName(..), Attribute (Label, Shape, HeadPort, TailPort), Label (HtmlLabel, StrLabel), Shape (..), PortPos (..))
import Data.List (intersperse)
import Data.GraphViz (DotNode (DotNode, nodeID, nodeAttributes), DotGraph (..), DotStatements (..), PrintDot (toDot), DotEdge (DotEdge))
import Data.GraphViz.Printing (renderDot)

import ParseGraph
import Utils (OnlyOne(..))
import Graph (Port, PortValue (..))
import qualified Graph as G

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

nodeString :: NodePortData -> String
nodeString = nodeIdToString . nodePortNodeId

portNodesToEdge :: PortNodes -> [DotEdge String]
portNodesToEdge pns = map makeEdge (toNodePorts pns) where
    makeEdge tNode = DotEdge (nodeString fNode) (nodeString tNode) ([headPort tNode, tailPort fNode] ++ labelList)
    fNode = case fromNodePort pns of
        OnlyOne fn -> fn
        NothingYet -> error "INVALID"
        TooMany -> error "INVALID"
    headPort n = HeadPort $ LabelledPort (PN . pack . nodePortId $ n) Nothing 
    tailPort n = TailPort $ LabelledPort (PN . pack . nodePortId $ n) Nothing 
    labelList = case (G.portValue . wirePort) pns of
        Just val -> [Label . StrLabel . pack . showValue $ val ]
        Nothing -> []
    showValue val = case val of
        BoolPortValue b -> show b
        IntPortValue i -> show i

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
