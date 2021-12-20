main :: IO()

main = do
    print $ sumRats (2, 5) (5, 2) == (29, 10)
    print $ sumRats (52, 123) (96, 14) == (6268, 861)
    print $ sumRats (2, 5) (3, 5) == (1, 1)

    print $ multiplyRats (2, 5) (5, 2) == (1, 1)
    print $ multiplyRats (52, 123) (96, 14) == (832, 287)
    print $ multiplyRats (2, 5) (3, 5) == (6, 25)

    print $ divideRats (2, 5) (5, 2) == (4, 25)
    print $ divideRats (52, 123) (96, 14) == (91, 1476)
    print $ divideRats (2, 5) (3, 5) == (2, 3)

    print $ areEqual (2, 5) (5, 2) == False
    print $ areEqual (52, 123) (52 * 3, 123 * 3) == True
    print $ areEqual (2, 6) (5, 15) == True

type Rat = (Int, Int)

normalize :: Rat -> Rat
normalize (x, 0) = error "Can't divide by zero"
normalize (x, y) = (div x g, div y g)
    where g = gcd x y

sumRats :: Rat -> Rat -> Rat
sumRats (x, y) (z, t) = normalize (x*t + y*z, y*t)

multiplyRats :: Rat -> Rat -> Rat
multiplyRats (x, y) (z, t) = normalize (x*z, y*t)

divideRats :: Rat -> Rat -> Rat
divideRats (x, y) (z, t) = multiplyRats (x, y) (t, z)

areEqual :: Rat -> Rat -> Bool
areEqual (x, y) (z, t) = normalize (x, y) == normalize (z, t)
