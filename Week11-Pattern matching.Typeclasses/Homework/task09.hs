main :: IO()

main = do
    print $ naturalProduct [-1, 0, -2, -3] 5 == 0 -- There are no natural numbers
    print $ naturalProduct [5, 10] 5 == 0 -- Sum of the divisors for 5 is 1 and for 10 is 1+2+5=8
    print $ naturalProduct [95, 75, 15, 55, 11, 14, 18, 35, 25] 5 == 1330

naturalProduct :: [Int] -> Int -> Int
naturalProduct xs k
 | null result = 0
 | otherwise = product result
    where
        result = [x | x <- xs, x > 0 && mod (sum [y | y <- [1 .. x-1], mod x y == 0]) k == 0]