-- Q1b
triangleType :: (Num, Num, Num) -> String
triangleType (a, b, c)
  | isRightAngle (a,b,c) = "Right Angle"
  | isEquilateral (a,b,c) = "Equilateral"
  | isIsosceles (a,b,c) = "Isosceles"
  | isScalene (a,b,c) = "Scalene"
  | otherwise = "Impossible"

isRightAngle (a,b,c)
  | (a^2 + b^2) == c^2 = True
  | (a^2 + c^2) == b^2 = True
  | (b^2 + c^2) == a^2 = True
  | otherwise          = False

isEquilateral (a,b,c)
  | a == b && b == c = True
  | otherwise = False

isIsosceles (a,b,c)
  | a == b && b /= c = True
  | a == c && b /= c = True
  | b == c && b /= a = True
  | otherwise        = False

isScalene (a,b,c)
  | a /= b && a /= c && b /= c = True
  | otherwise = False
