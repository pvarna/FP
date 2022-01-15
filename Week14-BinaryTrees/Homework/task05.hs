import Data.List

main :: IO()

main = do
    print $ levelSum numberBTree 1 == 11 -- (5 + 6)
    print $ cone numberBTree == True

data BTree = Nil | Node Int BTree BTree
 deriving (Show, Eq)

numberBTree = Node 10 (Node 5 (Node 1 Nil Nil) (Node 9 Nil Nil)) (Node 6 (Node 8 Nil Nil) (Node 7 Nil Nil))

getLevel :: BTree -> Int -> [Int]
getLevel Nil _ = []
getLevel (Node value _ _) 0 = [value]
getLevel (Node value left right) n = getLevel left (n - 1) ++ getLevel right (n - 1)

levelSum :: BTree -> Int -> Int
levelSum t l = (sum $ getLevel t l)

getSumsByLevel :: BTree -> [Int]
getSumsByLevel t = map sum (takeWhile (not . null) [ getLevel t x | x <- [0 ..]])  

cone :: BTree -> Bool
cone t = sums == (sort sums)
    where
        sums = getSumsByLevel t