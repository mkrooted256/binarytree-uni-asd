module L5
  ( Tree(EmptyTree)
  , singleton
  , insert
  , inTree
  , delete
  , treeMin
  , treeMax
  , popTreeMin
  ) where

data Tree t
  = EmptyTree
  | Node t (Tree t) (Tree t)
  deriving (Show)

singleton :: t -> Tree t
singleton x = Node x EmptyTree EmptyTree

insert :: (Ord t) => Tree t -> t -> Tree t
insert EmptyTree x = singleton x
insert (Node val left right) x
  | x >= val = Node val left (insert right x)
  | x < val = Node val (insert left x) right

treeMin :: (Ord t) => Tree t -> t
treeMin (Node val EmptyTree _) = val
treeMin (Node _ left _)        = treeMin left

treeMax :: (Ord t) => Tree t -> t
treeMax (Node val _ EmptyTree) = val
treeMax (Node _ _ right)       = treeMax right

popTreeMin :: (Ord t) => Tree t -> (t, Tree t)
popTreeMin (Node val EmptyTree right) = (val, right)
popTreeMin (Node val left right) = (minVal, Node val newLeft right)
  where
    (minVal, newLeft) = popTreeMin left

delete :: (Ord t) => Tree t -> t -> Tree t
delete EmptyTree x = error "Element not found"
delete (Node v EmptyTree left) x 
  | v == x = left
delete (Node v right EmptyTree) x 
  | v == x = right
delete (Node val left right) x
  | x > val = Node val left (delete right x)
  | x < val = Node val (delete left x) right
  | x == val = Node newVal left newRight
  where
    (newVal, newRight) = popTreeMin right

inTree :: (Ord t) => Tree t -> t -> Bool
inTree EmptyTree x = False
inTree (Node val left right) x
  | x == val = True
  | x > val = inTree right x
  | x < val = inTree left x

