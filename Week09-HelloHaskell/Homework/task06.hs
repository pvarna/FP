main :: IO()

main = do
    print $ isPrimeG 1 == False
    print $ isPrimeG 2 == True
    print $ isPrimeG 3 == True
    print $ isPrimeG 6 == False
    print $ isPrimeG 61 == True

    print $ isPrimeLC 1 == False
    print $ isPrimeLC 2 == True
    print $ isPrimeLC 3 == True
    print $ isPrimeLC 6 == False
    print $ isPrimeLC 61 == True

intSqrt :: Int -> Int
intSqrt = floor . sqrt . fromIntegral

getNumberOfDivisors :: Int -> Int
getNumberOfDivisors n = length (filter (\ x -> mod n x == 0) [2 .. intSqrt n])

isPrimeG :: Int -> Bool
isPrimeG n 
 | n == 1 = False
 | otherwise = getNumberOfDivisors n == 0

isPrimeLC :: Int -> Bool
isPrimeLC n = n /= 1 && length [ x | x <- [2 .. intSqrt n], mod n x == 0] == 0