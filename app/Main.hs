module Main (main) where

import ParseGraph (translateGraph)
import Examples (exampleOut)

main :: IO ()
main = do
    putStr $ unlines (translateGraph exampleOut)
    --print exampleOut
