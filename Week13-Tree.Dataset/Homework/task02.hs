main :: IO()

main = do
    print $ getAverage db1 == 4.457142857142857

    print $ getNeeded 750 db1 == [Product "Cheese" 750 5.0,Product "Water" 500 0.5,Product "Soap" 250 4.5]

    print $ closestToAverage db1 == ["Milk","Soap"]

    print $ cheaperAlternatives "Lamb" 5.50 db2 == 1
    print $ cheaperAlternatives "Lamb" 10  db2 == 2


type Name = String
type Quantity = Int
type Price = Double
type Database = [Product]

data Product = Product Name Quantity Price
 deriving (Show, Eq)

db1 :: Database
db1 = [ Product "Bread" 1000 1.20, Product "Milk" 2000 4.50, Product "Lamb" 5000 10.00, Product "Cheese" 750 5.00, Product "Butter" 1000 5.50, Product "Water" 500 0.50, Product "Soap" 250 4.50 ]

db2 :: Database
db2 = [ Product "Bread" 1000 1.20, Product "Milk" 2000 4.50, Product "Lamb" 5000 10.00, Product "Cheese" 750 5.00, Product "Lamb" 1000 5.50, Product "Water" 500 0.50, Product "Lamb" 250 4.50 ]

getTotal :: Database -> Price
getTotal = sum . map (\ (Product _ _ price) -> price) 

getAverage :: Database -> Price
getAverage db = getTotal db / (fromIntegral $ length db)

getNeeded :: Quantity -> Database -> Database
getNeeded _ [] = []
getNeeded quantity (p@(Product pName pQuantity pPrice):ps)
 | quantity < pQuantity = getNeeded quantity ps
 | otherwise = p : getNeeded quantity ps

closestToAverage :: Database -> [Name]
closestToAverage db = map (\ (Product name _ _) -> name) $ filter (\ (Product _ _ price) -> abs (price - averagePrice) == minPriceDiff) db
    where
        averagePrice = getAverage db
        minPriceDiff = minimum $ map (\ (Product _ _ price) -> abs (price - averagePrice)) db

cheaperAlternatives :: Name -> Price -> Database -> Int
cheaperAlternatives name price = length . filter (\ (Product pName _ pPrice) -> name == pName && pPrice < price)