main :: IO()

main = do
    print $ magicalProduct [2.7, 3.0 ..] 2.2 3 == -0.4399999999999998


magicalProduct :: [Double] -> (Double -> Int -> Double)
magicalProduct numbers = (\ x y -> foldl (\ acc z -> acc * (x - z)) 1 (take y numbers))