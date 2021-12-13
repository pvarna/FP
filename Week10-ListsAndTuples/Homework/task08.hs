import Data.Char;

main :: IO()

main = do
    print $ reduceStr "dabAcCaCBAcCcaDD" == "dabCBAcaDD" -- dabAcCaCBAcCcaDD -> dabAaCBAcCcaDD -> dabCBAcCcaDD -> dabCBAcaDD
                                                         --     ^^                 ^^                   ^^

areDuplicate :: Char -> Char -> Bool
areDuplicate x y = (x == toLower y || x == toUpper y) && x /= y 

findFirstDuplicate :: String -> Int -- function to find the index of the first occurrence of a duplicate, returns -1 if there aren't any duplicates
findFirstDuplicate xs = helper 0 xs
    where
        helper :: Int -> String -> Int
        helper _ [] = -1
        helper _ [x] = -1
        helper currentIndex (x:y:xs)
         | areDuplicate x y = currentIndex
         | otherwise = helper (currentIndex + 1) (y:xs)

removeDuplicateAt :: String -> Int -> String
removeDuplicateAt xs index = (take index xs) ++ (drop (index+2) xs)

reduceStr :: String -> String
reduceStr xs = helper xs
    where 
        helper :: String -> String
        helper current
         | findFirstDuplicate current == -1 = current -- the string is fine
         | otherwise = helper (removeDuplicateAt current (findFirstDuplicate current)) -- the string needs reduction

