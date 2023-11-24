module Main (main) where

import ParseGraph (translateGraph)
import Examples (exampleOut, test8, test9, test10, test11, test12, test13)
import Control.Category ((>>>))
import GraphVizUtils (buildDotGraph, serializeDotGraph)
import Graph (Graph)
import Data.Function ((&))

writeDotExample :: String -> Graph () () -> IO ()
writeDotExample fileName example = example & (
    exampleOut
    >>> translateGraph
    >>> uncurry buildDotGraph
    >>> serializeDotGraph
    >>> writeFile fileName
    )
main :: IO ()
main = do
    -- putStr $ unlines $ map show (fst $ translateGraph $ exampleOut test8)
    -- putStrLn ""
    --putStr $ unlines  (serializeGraph $ exampleOut test8)
    writeDotExample "graphs/test8.gv" test8
    writeDotExample "graphs/test9.gv" test9
    writeDotExample "graphs/test10.gv" test10
    writeDotExample "graphs/test11.gv" test11
    writeDotExample "graphs/test12.gv" test12
    writeDotExample "graphs/test13.gv" test13
    -- writeFile "graphs/test6.gv" . serializeDotGraph . uncurry buildDotGraph . translateGraph . exampleOut $ test8
    
