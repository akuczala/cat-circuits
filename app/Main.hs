module Main (main) where

import Lib
import ParseGraph (translateGraph)
import Conal (exampleOut)

main :: IO ()
main = do
    putStr $ unlines (translateGraph exampleOut)
    --print exampleOut
