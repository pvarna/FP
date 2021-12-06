main :: IO()

main = do
    print $ isInsideLists 5 1 4 == True
    print $ isInsideLists 10 50 20 == True
    print $ isInsideLists 10 50 1 == False

    print $ isInsideLambda 5 1 4 == True
    print $ isInsideLambda 10 50 20 == True
    print $ isInsideLambda 10 50 1 == False

isInsideLists :: Int -> Int -> Int -> Bool
isInsideLists a b x = elem x [min a b .. max a b]

isInsideLambda :: Int -> Int -> Int -> Bool
isInsideLambda a b = (\ x -> min a b <= x && x <= max a b)
