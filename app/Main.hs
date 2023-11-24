module Main (main) where

import ParseGraph (translateGraph)
import Examples (exampleOut, test8, test11)
import GraphVizUtils

main :: IO ()
main = do
    putStr $ unlines $ map show (fst $ translateGraph $ exampleOut test8)
    putStrLn ""
    --putStr $ unlines  (serializeGraph $ exampleOut test8)
    writeFile "graphs/test6.gv" . serializeDotGraph . uncurry buildDotGraph . translateGraph . exampleOut $ test8
    
