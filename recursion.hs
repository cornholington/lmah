
fib :: (Integral a) => a -> a
fib n
  | n == 1    = 1
  | n < 1     = 0
  | otherwise = (fib (n - 1)) + (fib (n - 2))

maximum' :: (Ord a) => [a] -> a
maximum' []  = error "empty list!"
maximum' [x] = x
--maximum' (x:xs)
--  | x > rest  = x
--  | otherwise = rest
--  where rest = maximum' xs

maximum' (x:xs) = max x (maximum' xs)

replicate' :: (Integral b) => b -> a -> [a]
replicate' n elem
  | n > 0     = elem:replicate' (n-1) elem
  | otherwise = []

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse xs ++ [x]

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (a:as) (b:bs) = (a,b):zip as bs

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' a (x:xs)
  | a == x    = True
  | otherwise = elem' a xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [a | a <- xs, a <= x] ++ x:quicksort[a | a <- xs, a > x]
