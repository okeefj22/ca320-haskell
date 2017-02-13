triangleArea :: Float -> Float -> Float -> Float             
triangleArea a b c
    | a + b + c <= 2 * maximum [a,b,c] = error "Not a triangle!"
    | otherwise                        = sqrt (s * (s - a) * (s - b) * (s - c))
        where s                        = (a + b + c) / 2


isSum :: Int -> Int -> Int -> Bool
isSum a b c
    | (a + b) == c = True
    | (a + c) == b = True
    | (b + c) == a = True
    | otherwise    = False
