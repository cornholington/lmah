module Locker
       ( Locker.lookup
       ) where

import qualified Data.Map as Map


data LockerState = Taken | Free deriving (Show, Eq)
type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

lookup :: Int -> LockerMap -> Either String Code
lookup n m = case Map.lookup n m of
  Nothing
    -> Left ("Locker number " ++ show n ++ " doesn't exist.")
  Just (state, code)
    -> if state /= Taken
       then Right code
       else Right ("Locker " ++ show n ++ " is already taken.")

lockers :: LockerMap
lockers = Map.fromList
    [(100,(Taken,"ZD39I"))
    ,(101,(Free,"JAH3I"))
    ,(103,(Free,"IQSA9"))
    ,(105,(Free,"QOTSA"))
    ,(109,(Taken,"893JJ"))
    ,(110,(Taken,"99292"))
    ]
