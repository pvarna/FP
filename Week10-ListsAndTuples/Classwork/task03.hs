main :: IO()

main = do
    print $ getPalindromesLC 132465 == 8
    print $ getPalindromesLC 654546 == 8
    print $ getPalindromesLC 100001 == 100012
    print $ getPalindromesLC 21612 == 21614
    print $ getPalindromesLC 26362 == 26364

    print $ getPalindromesHOF 132465 == 8
    print $ getPalindromesHOF 654546 == 8
    print $ getPalindromesHOF 100001 == 100012
    print $ getPalindromesHOF 21612 == 21614
    print $ getPalindromesHOF 26362 == 26364

getPalindromesLC :: Int -> Int
getPalindromesLC n = minimum divisors + maximum divisors
    where
        divisors :: [Int]
        divisors = [x | x <- [2 .. n], mod n x == 0 && (show x == (reverse $ show x))]

getPalindromesHOF :: Int -> Int
getPalindromesHOF n = minimum divisors + maximum divisors
    where   
        divisors :: [Int]
        divisors = filter (\ x -> mod n x == 0 && (show x == (reverse $ show x))) [2 .. n]