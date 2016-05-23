import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

numUniques :: (Eq a) => [a] -> Int
numUniques = length . List.nub


partition' :: (a -> Bool) -> [a] -> ([a],[a])
partition' f x = (filter f x, filter (not . f) x)

delete' :: (Eq a) => a -> [a] -> [a]
delete' a = foldr (\x acc -> if x == a then acc else x:acc) []

-- listDiff is my version of \\
-- lots of iterations to get it down, but things just kept collapsing
listDiff :: (Eq a) => [a] -> [a] -> [a]
listDiff = foldr delete'

-- my version of intersect
intersect' :: Eq a => [a] -> [a] -> [a]
intersect' _ [] = []
intersect' [] _ = []
intersect' a b  = foldr (\x acc -> if (elem x b) then x:acc else acc) [] a


-- insert takes an element and a list of elements that can be sorted and
-- inserts it into the last position where it's still less than or equal
-- to the next element
insert' :: Ord a => a -> [a] -> [a]
insert' x [] =  [x]
insert' x l@(y:ys)
  | x > y     = y:insert' x ys
  | otherwise = x:l


-- Let's make a function that looks up some value given a key.
findKey :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey key = foldr (\(k,v) acc -> if k == key then Just v else acc) Nothing
