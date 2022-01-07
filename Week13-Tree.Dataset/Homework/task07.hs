main :: IO()

main = do
    print $ (getOddCompositionValue [(\x -> x + 1),(\x -> x * 2),(\x -> x - 1), (\x -> div x 2)]) 2 == 2
    print $ (getOddCompositionValue [(\x -> x + 1)]) 2 == 3
    print $ (getOddCompositionValue [(\x -> x + 1),(\x -> x * 2),(\x -> x - 1), (\x -> div x 2), (\x -> x * 5)]) 2 == 10

getOddCompositionValue :: [(Int -> Int)] -> Int -> Int
getOddCompositionValue fs x = foldl (\ acc f -> f acc) x [fs!!n | n <- [0..length fs - 1], even n]