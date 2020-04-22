module Main where

import Text.Read
import L5
import Math

elementInput :: L5.Tree Int -> IO (L5.Tree Int)
elementInput tree = do
  putStrLn "Add element: "
  line <- getLine
  case readMaybe line :: Maybe Int of
    Just elem -> elementInput $ L5.insert tree elem
    _ -> do
      putStrLn "End of input"
      return tree

main :: IO ()
main = do
  tree <- elementInput L5.EmptyTree
  putStrLn "Tree:"
  putStrLn $ L5.showTree tree
  putStr "Enter element to find: "
  a <- getLine
  print $ L5.inTree tree $ read a 
  
  
