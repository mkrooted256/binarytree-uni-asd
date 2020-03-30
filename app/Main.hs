module Main where

import Text.Read
import L5

elementinput :: L5.Tree Int -> IO (L5.Tree Int)
elementinput tree = do
  putStrLn "Add element: "
  line <- getLine
  case readMaybe line :: Maybe Int of
    Just elem -> elementinput $ L5.insert elem tree
    _ -> do
      putStrLn "End of input"
      return tree

main :: IO ()
main = do
  tree <- elementinput L5.EmptyTree
  print tree

