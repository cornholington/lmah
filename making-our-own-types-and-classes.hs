--data Point = Point Float Float deriving (Show)
data Point = Point { x :: Float
                   , y :: Float
                   } deriving (Show)

-- illegal!?! member names collide with Point's
-- data Pointy = Pointy { x :: Float
--                      , y :: Float
--                      } deriving (Show)


data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = abs (x2 - x1) * (y2 - y1)

-- How about a function that nudges a shape? It takes a shape, the amount to
-- move it on the x axis and the amount to move it on the y axis and then
-- returns a new shape that has the same dimensions, only it's located
-- somewhere else.
nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) dx dy = Circle (Point (x+dx) (y+dy)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) dx dy = Rectangle (Point (x1+dx) (y1+dy)) (Point (x2+dx) (y2+dy))


data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     , height :: Float
                     , phoneNumber :: String
                     , flavor :: String
                     } deriving (Show)

data Vector a = Vector a a a deriving (Show)

vplus :: (Num t) => Vector t -> Vector t -> Vector t
vplus (Vector x y z) (Vector x1 y1 z1) = Vector (x+x1) (y+y1) (z+z1)

vmul :: (Num t) => Vector t -> t -> Vector t
vmul (Vector x y z) s = Vector (x*s) (y*s) (z*s)

smul :: (Num t) => Vector t -> Vector t -> t
smul (Vector x y z) (Vector x1 y1 z1) = (x*x1)+(y*y1)+(z*z1)

data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)


-- I don't understand Maybe and Either
--data EitherMaybe a b = (Maybe a) (Maybe b) | Nothing

--import qualified Data.Map as Map

-- our own list type, a recursive type
data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)
-- somewhat equivalent to:
-- data List a = Empty | Cons { listHead :: a, listTail :: List a} deriving (Show, Read, Eq, Ord)
