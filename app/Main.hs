module Main (main) where

import ParseGraph (translateGraph, serializeGraph)
import Examples (exampleOut)
import GraphVizUtils
import Data.GraphViz (PrintDot(..))
import Data.GraphViz.Printing (renderDot)
import Data.Text.Lazy (unpack)

main :: IO ()
main = do
    -- putStr $ unlines $ map show (fst $ translateGraph exampleOut)
    -- putStr $ unlines  (serializeGraph exampleOut)
    putStr $ unpack $ renderDot $ toDot (testGraph testNode)
    
