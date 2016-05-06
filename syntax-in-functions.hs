
mytake :: Int -> [a] -> [a]
mytake n [] = []
mytake n (x:xs)
     | n <= 0    = []
     | otherwise = x : mytake (n-1) xs

mycycle x = x ++ mycycle x



sayMe :: (Integral a) => a -> String
-- sayMe 1 = "One!"
-- sayMe 2 = "Two!"
-- sayMe 3 = "Three!"
-- sayMe 4 = "Four!"
-- sayMe 5 = "Five!"
-- sayMe x = "Not between 1 and 5"

sayMe x = case x of 1 -> "One!"
                    2 -> "Two!"
                    3 -> "Three!"
                    4 -> "Four!"
                    5 -> "Five!"
                    x  -> "Not between 1 and 5"


-- pattern matching tuples
first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

-- length in list comprehension
length' :: (Num b) => [a] -> b
length' x = sum [ 1 | _ <- x ]

-- length in pattern matching recursion
length'' :: (Num b) => [a] -> b
length'' [] = 0
length'' (_:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum xs

-- bmiTell :: (RealFloat a) => a -> a -> String
-- bmiTell weight height
--     | weight / height ^ 2 <= 18.5 = "You're underweight, you emo, you!"
--     | weight / height ^ 2 <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
--     | weight / height ^ 2 <= 30.0 = "You're fat! Lose some weight, fatty!"
--     | otherwise                 = "You're a whale, congratulations!"

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | bmi <= skinny = "You're underweight, you emo, you!"
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"
    | bmi <= fat    = "You're fat! Lose some weight, fatty!"
    | otherwise     = "You're a whale, congratulations!"
    where bmi = weight / height ^ 2
          (skinny, normal, fat) = (18.5, 25.0, 30.0)


mylt :: (Ord a) => a -> a -> Bool
x `mylt` y
  | x < y     = True
  | otherwise = False


-- ugly version:
-- initials :: String -> String -> String
-- initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
--     where (f:_) = firstname
--           (l:_) = lastname

initials :: String -> String -> String
initials (f:_) (l:_) = f:"." ++ l:"."

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea  = pi * r ^2
        foo      = "bar"
    in  if (foo == "bar") then sideArea + 2 * topArea else 0

-- where used to define a local function, "what"
describeList :: [a] -> String
describeList xs = "The list is " ++ what xs
    where what [] = "empty."
          what [x] = "a singleton list."
          what xs = "a longer list."
