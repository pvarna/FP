main :: IO()

main = do
    print $ removeFirst 5 [5, 1, 5, 3, 5] == [1, 5, 3, 5]
    print $ removeFirst 3 [5, 1, 5, 3, 5] == [5, 1, 5, 5]

removeFirst :: Int -> [Int] -> [Int]
removeFirst _ [] = error "The number isn't present in the list" -- maybe should return [], i'm not sure
removeFirst n (x:xs)
 | n == x = xs
 | otherwise = x : removeFirst n xs