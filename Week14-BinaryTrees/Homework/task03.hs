main :: IO()

main = do
    print $ toBinaryIndexed tree   == Node (10,5) (Node (5,2) (Node (3,1) (Node (1,0) Nil Nil) Nil) (Node (7,4) (Node (6,3) Nil Nil) Nil)) (Node (15,7) (Node (13,6) Nil Nil) (Node (18,8) Nil Nil))
   
data BTree a = Nil | Node a (BTree a) (BTree a)
 deriving (Show, Eq)

tree = Node 10 (Node 5 (Node 3 (Node 1 Nil Nil) Nil) (Node 7 (Node 6 Nil Nil) Nil)) (Node 15 (Node 13 Nil Nil) (Node 18 Nil Nil))

getValuesDFS :: BTree a -> [a]
getValuesDFS Nil = []
getValuesDFS (Node value left right) = getValuesDFS left ++ [value] ++ getValuesDFS right

elemIndex :: Int -> [Int] -> Int
elemIndex elem xs = helper elem 0 xs
    where
        helper :: Int -> Int -> [Int] -> Int
        helper _ _ [] = -1
        helper el index (y:ys)
         | el == y = index
         | otherwise = helper el (index + 1) ys


toBinaryIndexed :: BTree Int -> BTree (Int,Int)
toBinaryIndexed tree = helper tree tree
    where
        helper :: BTree Int -> BTree Int -> BTree (Int,Int)
        helper _ Nil = Nil
        helper original current@(Node value left right) = (Node (value,(elemIndex value (getValuesDFS original))) (helper original left) (helper original right))