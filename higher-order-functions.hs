-- haskell is stupid about '-'
-- (*1) is a function that multiplies by one
-- (/1) is a function that divides by one
-- (+1) is a function that adds one
-- (-1) __is negative one__
-- take -1 "hi" --> error!
--
minus :: (Num a) => a -> a -> a
minus = (-)

minus3 :: (Num a) => a -> a
minus3 = (`minus` 3)

-- goal:
--  minus3 2 => -1

multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z


applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y:zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f x y = f y x

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
--map' f (x:xs) = f x:map' f xs
map' f x = [f a | a <- x ]

--filter' :: (a -> Bool) -> [a] -> [a]
--filter' _ [] = []
--filter' f (x:xs)
--  | f x       = x:filter' f xs
--  | otherwise = filter' f xs
--filter' f x = [ a | a <- x, f a == True ]

quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs) = quicksort' (filter' (<=x) xs) ++ [x] ++ quicksort' (filter' (>x) xs)

-- find the largest number under b that's evenly divisible by a.
largestDivisibleByUnder :: Int -> Int -> Int
largestDivisibleByUnder a b = head (filter' (\x -> (mod x a) == 0) [b,b-1..])

-- find the sum of all odd squares that are smaller than y
sumOfAllOddSquares :: Int -> Int
sumOfAllOddSquares y = sum (takeWhile (<y) (map' (^2) [1,3..y]))

-- Collatz sequences
-- We take a natural number. If that number is even, we divide it by two. If
-- it's odd, we multiply it by 3 and then add 1 to that. We take the resulting
-- number and apply the same thing to it, which produces a new number and so
-- on. In essence, we get a chain of numbers. It is thought that for all
-- starting numbers, the chains finish at the number 1. So if we take the
-- starting number 13, we get this sequence: 13, 40, 20, 10, 5, 16, 8, 4, 2,
-- 1. 13*3 + 1 equals 40. 40 divided by 2 is 20, etc. We see that the chain has
-- 10 terms.
chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain x
  | even x    = x:chain (div x 2)
  | otherwise = x:chain (x * 3 + 1)

-- for all starting numbers between 1 and x, how many chains have a length greater than y?
numLongChains :: Int -> Int -> Int
numLongChains x y = length (filter' (\x ->length x>y) (map' chain [1..x]))


-- lambdas
addThree :: (Num a) => a -> a -> a -> a
-- ug, don't get it... no help in lyah
addThree = (\x -> (\y -> (\z -> x + y + z)))

--  left fold. It folds the list up from the left side. The binary function is
--  applied between the starting value and the head of the list. That produces a
--  new accumulator value and the binary function is called with that value and
--  the next element, etc.
foldl' :: (a -> a -> a) -> a -> [a] -> a
foldl' _ a []     = a
foldl' f a (x:xs) = foldl' f (f a x) xs

--head' :: [a] -> a
--head' [] = error "head': empty list"
--head' (x:xs) = x

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ []     = []
takeWhile' f (x:xs)
           | f x == True = x:takeWhile' f xs
           | otherwise   = []

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' x (y:ys)
  | x == y    = True
  | otherwise = elem' x ys

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ a []     = a
foldr' f a (x:xs) = f x (foldr' f a xs)

maximum' :: (Ord a) => [a] -> a
maximum' = foldr1 (\x y -> max x y)

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []

product' :: (Num a) => [a] -> a
product' = foldl (\acc x -> x * acc) 1

filter' :: (a -> Bool) -> [a] -> [a]
--filter' f = foldr (\x acc -> (if (f x) then x:acc else acc)) []
filter' f = foldl (\acc x -> (if (f x) then (acc++[x]) else acc)) []

head' :: [a] -> a
head' = foldr1 (\x _ -> x)

last' :: [a] -> a
last' = foldl1 (\acc _ -> acc)


scanl' :: (b -> a -> b) -> b -> [a] -> [b]
scanl' _ a [] = [a]
scanl' f a (x:xs) = a:scanl' f (f a x) xs

-- I suck at recursion
-- gregf
---- It's not that you suck at recursion. You apparently just suck at lazy
---- evaluation. Rather than building bottom-up like you did with scanl, can
---- you find the N-1 case?
scanr' :: (a -> b -> b) -> b -> [a] -> [b]
scanr' _ a [] = [a]
--scanr' f a xs = (scanr' f (f (last xs) a) (init xs))++[a]
scanr' f a (x:xs) =
  let rest = (scanr' f a xs)
  in f x (head rest):rest




-- Let's answer us this question: How many elements does it take for the sum of
-- the roots of all natural numbers to exceed 1000?
sumOfRootsExceeds :: Double -> Int
sumOfRootsExceeds x = length (takeWhile (<x) (scanl' (\a x -> a + sqrt x) 0 [1..]))

sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1

-- for function application...
-- map ($ 3) [(4+), (10*), (^2), sqrt]
-- [7.0,30.0,9.0,1.7320508075688772]

-- Say we have a list of numbers and we want to turn them all into negative
-- numbers
negList :: (Num a, Ord a) => [a] -> [a]
--negList = map (\x -> if x > 0 then (negate x) else x)
--negList = map (\x -> negate (abs x))
--negList = map (\x -> negate $ abs x)
negList = map (negate . abs)

-- how can we write this in point-free style?
--fn x = ceiling (negate (tan (cos (max 50 x))))
-- fn :: (Integral a, Floating a, Ord a) => a -> a
fn = ceiling . negate . tan . cos . max 50
fn' x = ceiling (negate (tan (cos (max 50 x))))
