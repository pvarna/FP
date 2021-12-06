main :: IO()

main = do
    print $ hasIncDigits 1244 == True
    print $ hasIncDigits 12443 == False

hasIncDigits :: Int -> Bool
hasIncDigits n
 | n < 0 = error "n was negative"
 | otherwise = helper n
 where
    helper :: Int -> Bool
    helper leftOver
     | leftOver < 10 = True
     | (mod leftOver 10) < (mod (div leftOver 10) 10) = False
     | otherwise = helper (div leftOver 10)