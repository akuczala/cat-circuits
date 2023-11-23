module Main (main) where

import ParseGraph (translateGraph)
import Examples (exampleOut, test8)
import GraphVizUtils

main :: IO ()
main = do
    putStr $ unlines $ map show (fst $ translateGraph $ exampleOut test8)
    putStrLn ""
    --putStr $ unlines  (serializeGraph $ exampleOut test8)
    putStr . serializeDotGraph . uncurry buildDotGraph . translateGraph . exampleOut $ test8
    
