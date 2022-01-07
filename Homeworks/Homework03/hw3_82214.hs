import Data.List

main :: IO()

main = do
    print $ willItFly [1, 4, 2, 3] == True
    print $ willItFly [1, 4, 2, -1, 6] == False
    print $ willItFly [1] == True
    --------------------------------------------------
    print $ formatDuration 0 == "now"
    print $ formatDuration 1 == "1 second"
    print $ formatDuration 62 == "1 minute and 2 seconds"
    print $ formatDuration 120 == "2 minutes"
    print $ formatDuration 3600 == "1 hour"
    print $ formatDuration 3662 == "1 hour, 1 minute and 2 seconds"
    print $ formatDuration 86400 == "1 day"
    print $ formatDuration (86400+14400+1920+11) == "1 day, 4 hours, 32 minutes and 11 seconds"
    print $ formatDuration (2*31536000+86400+14400+1920+11) == "2 years, 1 day, 4 hours, 32 minutes and 11 seconds"

willItFly :: [Int] -> Bool
willItFly [] = error "no presents to order"
willItFly (z:zs) = (sort $ zipWith (\ x y -> abs (x - y)) (z:zs) zs) == [1 .. (length (z:zs) - 1)]

--------------------------------------------------

divs = [60, 60, 24, 365]
timeUnits = ["second", "minute", "hour", "day", "year"]

getValues :: Int -> [Int]
getValues n = helper n divs []
    where 
        helper :: Int -> [Int] -> [Int] -> [Int]
        helper leftOver [] result = result ++ [leftOver]
        helper leftOver (x:xs) result = helper (div leftOver x) xs (result ++ [(mod leftOver x)])

unitToStr :: Int -> String -> String
unitToStr 0 _ = ""
unitToStr 1 str = (show 1) ++ " " ++ str ++ ""
unitToStr n str = (show n) ++ " " ++ str ++ "s"

addUnits :: [Int] -> [String]
addUnits xs = reverse $ filter (not . null) (map (\ (value, unit) -> unitToStr value unit) (zip xs timeUnits)) 

toSingleStr :: [String] -> String
toSingleStr [x] = x
toSingleStr xs = intercalate ", " (init xs) ++ " and " ++ (last xs)

formatDuration :: Int -> String
formatDuration 0 = "now"
formatDuration n
 | n < 0 = error "invalid time"
 | n > 0 = toSingleStr $ addUnits $ getValues n