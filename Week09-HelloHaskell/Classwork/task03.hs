main :: IO()

main = do
    print $ factXs 11 == 39916800
    print $ factRec 11 == 39916800
    -- print $ factRec (-11) -- error: x was negative
    print $ factIter 11 == 39916800
    --print $ factIter (-11) -- error: x was negative

factXs :: Int -> Int
factXs x
 | x < 0 = error "x was negative"
 | otherwise = product [1 .. x]

factRec :: Int -> Int
factRec 0 = 1
factRec 1 = 1
factRec x 
 | x < 0 = error "x was negative"
 | otherwise = x * factRec(x - 1)

factIter :: Int -> Int
factIter x 
 | x < 0 = error "x was negative"
 | otherwise = helper x 1
 where
    helper :: Int -> Int -> Int
    helper 1 result = result
    helper leftOver result = helper (leftOver - 1) (result * leftOver)
