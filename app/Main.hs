module Main (main) where

import Examples (writeDotExample, examples)
import Control.Monad (zipWithM_)

fileNames :: [String]
fileNames = map (\i -> "graphs/example" ++ show i ++ ".gv") ([0..] :: [Integer])

main :: IO ()
main = do
    zipWithM_ writeDotExample fileNames examples
    
