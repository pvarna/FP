import Data.List;

main :: IO()

main = do
    print $ reverseOrdSuff 37563 == 36
    print $ reverseOrdSuff 32763 == 367
    print $ reverseOrdSuff 32567 == 7
    print $ reverseOrdSuff 32666 == 6


reverseOrdSuff :: Int -> Int
reverseOrdSuff n = read $ last $ filter (\ x -> x == (sort x)) (inits $ nub $ reverse $ show n)