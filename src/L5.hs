module L5
    ( Tree(EmptyTree),
      singleton,
      insert,
      intree
    ) where


data Tree t = EmptyTree | Node t (Tree t) (Tree t) deriving (Show)

singleton :: t -> Tree t  
singleton x = Node x EmptyTree EmptyTree  

insert :: (Ord t) => t -> Tree t -> Tree t
insert x EmptyTree = singleton x
insert x (Node val left right)
  | x >= val = Node val left (insert x right) 
  | x < val  = Node val (insert x left) right

intree :: (Ord t) => t -> Tree t -> Bool
intree x EmptyTree = False
intree x (Node val left right)
  | x == val = True
  | x > val  = intree x right
  | x < val  = intree x left