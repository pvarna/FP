main :: IO()

main = do
    print $ convert tree == (Node 30 (Node 36 (Node 36 Nil Nil) (Node 35 Nil (Node 33 Nil Nil))) (Node 21 (Node 26 Nil Nil) (Node 15 Nil (Node 8 Nil Nil))))

data BTree = Nil | Node Int BTree BTree
 deriving (Show, Eq)

tree = Node 4 (Node 1 (Node 0 Nil Nil) (Node 2 Nil (Node 3 Nil Nil))) (Node 6 (Node 5 Nil Nil) (Node 7 Nil (Node 8 Nil Nil)))

getValuesDFS :: BTree -> [Int]
getValuesDFS Nil = []
getValuesDFS (Node value left right) = getValuesDFS left ++ [value] ++ getValuesDFS right

toNewValue :: Int -> BTree -> Int
toNewValue n tree = sum $ filter (>= n) (getValuesDFS tree) 

convert :: BTree -> BTree
convert tree = helper tree tree
    where
        helper :: BTree -> BTree -> BTree
        helper _ Nil = Nil
        helper original (Node value left right) = (Node (toNewValue value original) (helper original left) (helper original right))