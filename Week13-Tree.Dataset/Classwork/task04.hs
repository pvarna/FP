import Data.List

main :: IO()

main = do
    print $ t
    print $ isBST t == True

data BTree = Nil | Node Int BTree BTree
 deriving (Show, Eq) 

t = Node 7 (Node 3 (Node 1 Nil Nil) (Node 5 Nil Nil)) (Node 10 Nil (Node 14 Nil Nil))

traverseDFS :: BTree -> [Int]
traverseDFS Nil = []
traverseDFS (Node value left right) = traverseDFS left ++ [value] ++ traverseDFS right

isBST :: BTree -> Bool
isBST tree = nodes == sort nodes
    where nodes = traverseDFS tree