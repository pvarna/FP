main :: IO()

minIf :: Int -> Int -> Int

minGuard :: Int -> Int -> Int

minBuiltIn :: Int -> Int -> Int

lastDigit :: Int -> Int

quotientWhole :: Int -> Int -> Int

divWhole :: Int -> Int -> Double

removeLastDigit :: Int -> Int

divReal :: Double -> Double -> Double

quotientReal :: Double -> Double -> Int

avgWhole :: Int -> Int -> Double

roundTwoDig :: Double -> Double

roundTwoDigButWithMagic :: Double -> Double

main = do
    print $ minIf 15 60 == 15
    print $ minIf 60 15 == 15
    print $ minGuard 15 60 == 15
    print $ minGuard 60 15 == 15
    print $ minBuiltIn 60 15 == 15

    print $ lastDigit 154 == 4

    print $ quotientWhole 64 2 == 32
    print $ divWhole 154 17 == 9.058823529411764

    print $ removeLastDigit 154 == 15    

    print $ divReal 154.451 10.01 == 15.42967032967033
    print $ quotientReal 154.21 17.17 == 9

    print $ avgWhole 5 1542 == 773.5

    print $ roundTwoDig 3.1413465345321 == 3.14
    print $ roundTwoDigButWithMagic 3.1413465345321 == 3.14 -- partial function application and composition (defining a function at functional level)

minIf x y = if x < y then x else y

minGuard x y
 | x < y = x
 | otherwise = y

minBuiltIn x y = min x y

lastDigit n = mod n 10

quotientWhole x y = div x y

divWhole x y = (fromIntegral x) / (fromIntegral y)

removeLastDigit n = div n 10

divReal x y = x / y

quotientReal x y = round (x / y)

avgWhole x y = (fromIntegral (x + y)) / 2

roundTwoDig n = (fromIntegral $ round $ n * 100) / 100 

roundTwoDigButWithMagic = (/ 100) . fromIntegral . round . (* 100)
