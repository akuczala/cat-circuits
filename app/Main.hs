module Main (main) where

import ParseGraph (translateGraph, serializeGraph)
import Examples (exampleOut)

main :: IO ()
main = do
    putStr $ unlines $ map show (fst $ translateGraph exampleOut)
    putStr $ unlines  (serializeGraph exampleOut)
    
