module Main where

import System.Environment

import CM.CaseStudy.Instances.Manual as CSInstance
import CM.CaseStudy.Instances.GeneratedPeople as CSGP
import CM.Visualization

main :: IO ()
main = do
  args <- getArgs
  if length args < 1 then putStrLn . modelToDotModel $ CSInstance.model
  else case head args of
    "manual" -> putStrLn . modelToDotInstance $ CSInstance.model
    "generated" -> putStrLn . modelToDotInstance $ CSGP.generatedPeopleModel
    _ -> putStrLn . modelToDotModel $ CSInstance.model
