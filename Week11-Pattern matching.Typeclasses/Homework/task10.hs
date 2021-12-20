main :: IO()

main = do
    print $ crossProduct (1, 2, 3) (7, 4, 1) == (-10, 20, -10)
    print $ crossProduct (5, 2, 159) (0, -1, -2) == (155, 10, -5)

    print $ magnitude (1, 2, 3) == 3.7416573867739413
    print $ magnitude (7, 4, 1) == 8.12403840463596
    print $ magnitude (-10, 20, -10) == 24.49489742783178
    print $ magnitude (5, 2, 159) == 159.0911688309568
    print $ magnitude (0, -1, -2) == 2.23606797749979
    print $ magnitude (155, 10, -5) == 155.40270267920053

type Vector a = (a, a, a)

crossProduct :: (Num a) => Vector a -> Vector a -> Vector a
crossProduct (u1, u2, u3) (v1, v2, v3) = (u2*v3 - u3*v2, u3*v1 - u1*v3, u1*v2 - u2*v1)

magnitude :: (Floating a) => Vector a -> a
magnitude (x, y, z) = sqrt (x*x + y*y + z*z)