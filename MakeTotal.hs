module MakeTotal where

import Data.List(sort)

{-

divTwo 11 = [[5,6], [4,7], [3,8], [2,9], [1,10]]

-}

divTwo' :: Int -> Int -> [[Int]]
divTwo' count num
  | (div num 2) - count == 0 = []
  | otherwise = [ a, (num - a)] : divTwo' (count + 1) num
  where
    a = (div num 2) - count

divTwo = divTwo' 0

regroup :: ([a],[a]) -> [a]
regroup ([a,b],[c,d]) = [a,b,c,d]


{-

makeTotal 23 

make a total of 23 with 4 elements:

[[6,5,6,6], [7,4,6,6], [8,3, 6, 6], [9,2, 6, 6], [10, 1, 6, 6], [2,3,7,11], [4,5,6,8]...]

-}

makeTotal ::  Int -> [[Int]]
makeTotal total =  sort $ map sort $ map regroup [(x,y) | x <- (divTwo a), y <- (divTwo b)]
  where
    a = div total 2
    b = total - a
