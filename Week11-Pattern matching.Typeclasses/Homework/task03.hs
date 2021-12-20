main :: IO()

main = do
    print $ removeAll 5 [5] == []
    print $ removeAll 4 [4, 4] == []
    print $ removeAll 5 [1] == [1]
    print $ removeAll 5 [5, 1, 5, 3, 5] == [1, 3]
    print $ removeAll 3 [5, 1, 5, 3, 5] == [5, 1, 5, 5]
    -- same for removeAllHOF
    print $ removeAllHOF 5 [5] == []
    print $ removeAllHOF 4 [4, 4] == []
    print $ removeAllHOF 5 [1] == [1]
    print $ removeAllHOF 5 [5, 1, 5, 3, 5] == [1, 3]
    print $ removeAllHOF 3 [5, 1, 5, 3, 5] == [5, 1, 5, 5]
    -- same for removeAllLC ---> bonus, this one is the easiest one for me
    print $ removeAllLC 5 [5] == []
    print $ removeAllLC 4 [4, 4] == []
    print $ removeAllLC 5 [1] == [1]
    print $ removeAllLC 5 [5, 1, 5, 3, 5] == [1, 3]
    print $ removeAllLC 3 [5, 1, 5, 3, 5] == [5, 1, 5, 5]
    

removeAll :: Int -> [Int] -> [Int]
removeAll _ [] = []
removeAll n (x:xs)
 | n == x = removeAll n xs
 | otherwise = x : removeAll n xs


removeAllHOF :: Int -> [Int] -> [Int]
removeAllHOF n xs = filter (\ x -> x /= n) xs

removeAllLC :: Int -> [Int] -> [Int]
removeAllLC n xs = [x | x <- xs, x /= n]