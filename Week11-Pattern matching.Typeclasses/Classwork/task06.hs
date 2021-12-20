main :: IO()

main = do
    print $ normalize (4, 2) == (2, 1)
    print $ normalize (8, 4) == (2, 1)
    print $ normalize (2, 4) == (1, 2)

type Rat = (Int, Int)

normalize :: Rat -> Rat
normalize (x, 0) = error "Can't divide by zero"
normalize (x, y) = (div x g, div y g)
    where g = gcd x y