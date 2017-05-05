module Main where

import CM.CaseStudy.Instances.Manual as CSInstance
import CM.Visualization

main :: IO ()
main = do
  putStrLn . modelToDotInstance $ CSInstance.model
  --putStrLn . modelToDotModel $ CSInstance.model
