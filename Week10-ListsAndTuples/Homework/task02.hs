import Data.Char

main :: IO()

main = do
    print $ sumSpecialPrimes 5 2 == 392
    print $ sumSpecialPrimes 5 3 == 107
    print $ sumSpecialPrimes 10 3 == 462

intSqrt :: Int -> Int
intSqrt = floor . sqrt . fromIntegral

isPrime :: Int -> Bool
isPrime n = n /= 1 && length [ x | x <- [2 .. intSqrt n], mod n x == 0] == 0

containsD :: Int -> Int -> Bool
containsD n d = elem d $ map digitToInt $ show n

sumSpecialPrimes :: Int -> Int -> Int
sumSpecialPrimes n d = sum $ take n [ x | x <- [2 ..], isPrime x && containsD x d]

