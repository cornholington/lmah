factorial :: Integer -> Integer
factorial n = product [1..n]

removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [ c | c <- st, elem c ['A'..'Z'] ]

circumference :: Float -> Float
circumference r = 2 * pi * r

circumference' :: Double -> Double
circumference' r = 2 * pi * r

myelem :: (Eq a) => a -> [a] -> Bool
myelem x l = not (null [ y | y <- l, y == x ])

mylt :: (Ord a) => a -> a -> Bool
mylt x y = (compare x y) == LT

-- fun to use, try length allInts
allInts :: [Int]
allInts = [minBound..maxBound]
