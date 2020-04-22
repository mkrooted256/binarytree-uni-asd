module L5
  ( Tree(EmptyTree)
  , singleton
  , insert
  , inTree
  , delete
  , treeMin
  , treeMax
  , popTreeMin
  , showTree
  ) where

import           Data.Char
import           Text.ParserCombinators.ReadP

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

treeDepth :: Tree t -> Int
treeDepth EmptyTree    = 0
treeDepth (Node v l r) = max 1 (max (treeDepth l) (treeDepth r))

showTree :: (Show a) => Tree a -> String
showTree = showTreeHelper 0
  where
    showTreeHelper :: (Show a) => Int -> Tree a -> String
    showTreeHelper _ EmptyTree = ""
    showTreeHelper depth (Node v l r) =
      showTreeHelper (depth + 1) r ++ indent depth ++ show v ++ "\n" ++ showTreeHelper (depth + 1) l
      where
        indent :: Int -> String
        indent n = concat $ replicate n "|     "

-- -------------------------------------------------- --
-- -------------------------------------------------- --
data Token
  = Val Int
  | Op Char
  deriving (Show) -- | LB | RB

applyOp :: Char -> Int -> Int -> Int
applyOp '+' = (+)
applyOp '-' = (-)
applyOp '*' = (*)
applyOp '^' = (^)
applyOp '/' = div

--
compute :: Tree Token -> Int
compute (Node (Val a) EmptyTree EmptyTree) = a
compute (Node (Op c) l r)                  = applyOp c (compute l) (compute r)

infixTraverse :: Tree Token -> (String, Int)
infixTraverse (Node (Val a) EmptyTree EmptyTree) = (show a, a)
infixTraverse (Node (Op c) l r) = ("(" ++ lhs ++ " " ++ [c] ++ " " ++ rhs ++ ")",  applyOp c lval rval) 
  where
    (lhs, lval) = infixTraverse l
    (rhs, rval) = infixTraverse r

prefixTraverse :: Tree Token -> (String, Int)
prefixTraverse (Node (Val a) EmptyTree EmptyTree) = (show a, a)
prefixTraverse (Node (Op c) l r) = ([c] ++ " " ++ lhs ++ " " ++ rhs, applyOp c lval rval)
  where
    (lhs, lval) = prefixTraverse l
    (rhs, rval) = prefixTraverse r

postfixTraverse :: Tree Token -> (String, Int)
postfixTraverse (Node (Val a) EmptyTree EmptyTree) = (show a, a)
postfixTraverse (Node (Op c) l r) = (lhs ++ " " ++ rhs ++ " " ++ [c], applyOp c lval rval)
  where
    (lhs, lval) = postfixTraverse l
    (rhs, rval) = postfixTraverse r

sampleExpression =
  Node
    (Op '+')
    (singleton (Val 9))
    (Node
       (Op '*')
       (singleton (Val 8))
       (Node
          (Op '+')
          (Node
             (Op '+')
             (singleton (Val 7))
             (Node
                (Op '-')
                (Node (Op '*') (singleton (Val 6)) (Node (Op '+') (singleton (Val 5)) (singleton (Val 4))))
                (Node (Op '-') (singleton (Val 3)) (singleton (Val 2)))))
          (singleton (Val 1))))



--
--
--
--lexer :: String -> [Token]
--lexer [] = []
--lexer ('(' : tail) = LB : lexer tail
--lexer (')' : tail) = RB : lexer tail
--lexer str@(c:tail)
--  | c `elem` "+-*^/" = Op c : lexer tail
--  | isSpace c = lexer tail
--  | isDigit c = Val val : lexer parseTail where
--    ((val, parseTail):_) = reads str
--lexer (a:_) = error $ "Invalid char in math expression: " ++ show a
--
--type Expression = Tree Token
--
--parseVal :: [Token] -> Maybe(Expression, [Token])
--parseVal (Val val : tail) = Just (singleton val, tail)
--parseVal _ = Nothing
--
--parseTrivialOp :: [Token] -> Maybe(Expression, [Token])
--parseTrivialOp tokens = case parseVal tokens of
--  Just (lhs, Op c : tail1) -> case parseTrivialOp tail1 of
--    Just (rhs, tail2) -> Just (Node (Op c) lhs rhs, tail2)
--
--parseExpr :: [Token] -> Expression
--parseExpr
--statement :: ReadP Statement
--statement = do
--  l <- statement
--  op <- operator
--  r <- statement
