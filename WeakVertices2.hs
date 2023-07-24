module Main where

import Data.List (sort, nub, (\\), intercalate)
import Control.Monad (replicateM, unless)

-- list edges in a row
rowToCol :: Int -> Int -> [Int] -> [(Int,Int)]
rowToCol _ _ [] = []
rowToCol row col (x:xs)
  | x == 1         = (row,col) : rowToCol row (col+1) xs
  | otherwise = rowToCol row (col+1) xs

allEdges :: Int -> Int -> [[Int]] -> [[(Int,Int)]]
allEdges _ _ [] = []
allEdges row col (x:xs) =  rowToCol row col x : allEdges (row + 1) 0 xs

flattenAllEdges :: [[Int]] -> [(Int,Int)]
flattenAllEdges xs = concat (allEdges 0 0 xs)

triangle :: Int -> (Int, Int) -> [(Int,Int)] -> (Int, Int, Int, Bool)
triangle n (a, b) xs
  | elem  (n , a) xs && elem (n , b) xs && elem (a, b) xs = (n, a, b, True)
  | otherwise                                             = (n, -1, -1,False)

allPairs :: Int -> [(Int, Int)]
allPairs n = [(a, b) | a <- [0..n], b <- [0..n]]

applyTriangleOnce :: Int -> Int -> [[Int]] -> [(Int, Int, Int, Bool)]
applyTriangleOnce n max input = map (\x -> triangle n x (flattenAllEdges input)) (allPairs max)

getBool  :: (Int, Int, Int, Bool) -> Bool
getBool (a, b, c, x) = x

sortTriangle :: (Int, Int, Int, Bool) -> Bool
sortTriangle (a, b, c, x) = a < b && b < c

filterTriangles :: [(Int, Int, Int, Bool)] -> [(Int, Int, Int, Bool)]
filterTriangles [] = []
filterTriangles (x:xs)
  | getBool x  && sortTriangle x  = x : filterTriangles xs
  | otherwise                                 = filterTriangles xs

getTriangles :: [[Int]] -> [(Int, Int, Int, Bool)]
getTriangles xs = concatMap (\x -> filterTriangles $ applyTriangleOnce x z xs) [0..z]
  where
    z = length xs - 1

getInts :: (Int, Int, Int, Bool) -> [Int]
getInts  (a,b,c,x) = [a,b,c]

getStrong :: [[Int]] -> [Int]
getStrong xs = nub $ concatMap getInts (getTriangles xs)

getWeak :: [[Int]] -> [Int]
getWeak xs = (\\) [0..len] (getStrong xs)
  where
    len = length xs - 1

getRow :: IO [Int]
getRow = do
  map (read::String -> Int) . words <$> getLine

-- Function to read the adjacency matrix of size n x n
readAdjacencyMatrix :: Int -> IO [[Int]]
readAdjacencyMatrix n = replicateM n $ do
  map read . words <$> getLine

listToString :: [Int] -> String
listToString xs = unwords [show x | x <- xs]

-- Function to process the adjacency matrix
processMatrix :: [[Int]] -> IO ()
processMatrix matrix = do
  putStrLn $ listToString (getWeak matrix)

main :: IO ()
main = do
  n <- readLn -- Read the size of the matrix (n x n)
  unless (n == -1) $ do
    adjacencyMatrix <- readAdjacencyMatrix n
    processMatrix adjacencyMatrix
    main -- Continue reading and processing matrices recursively


-- * input
{-
9
0 1 1 1 0 0 0 0 0
1 0 0 0 0 0 1 0 0
1 0 0 1 0 1 0 0 0
1 0 1 0 0 1 1 0 0
0 0 0 0 0 0 1 1 0
0 0 1 1 0 0 0 0 0
0 1 0 1 1 0 0 1 0
0 0 0 0 1 0 1 0 1
0 0 0 0 0 0 0 1 0
1
0
-1

----------------------------
input1 :: [[Int]]
input1 =
  [[0, 1, 1, 1, 0, 0, 0, 0, 0],
   [1, 0, 0, 0, 0, 0, 1, 0, 0],
   [1, 0, 0, 1, 0, 1, 0, 0, 0],
   [1, 0, 1, 0, 0, 1, 1, 0, 0],
   [0, 0, 0, 0, 0, 0, 1, 1, 0],
   [0, 0, 1, 1, 0, 0, 0, 0, 0],
   [0, 1, 0, 1, 1, 0, 0, 1, 0],
   [0, 0, 0, 0, 1, 0, 1, 0, 1],
   [0, 0, 0, 0, 0, 0, 0, 1, 0]]
---------------------------------------------
[[0, 1, 1, 1, 0, 0, 0, 0, 0],
[1, 0, 0, 0, 0, 0, 1, 0, 0],
[1, 0, 0, 1, 0, 1, 0, 0, 0],
[1, 0, 1, 0, 0, 1, 1, 0, 0],
[0, 0, 0, 0, 0, 0, 1, 1, 0],
[0, 0, 1, 1, 0, 0, 0, 0, 0],
[0, 1, 0, 1, 1, 0, 0, 1, 0],
[0, 0, 0, 0, 1, 0, 1, 0, 1],
[0, 0, 0, 0, 0, 0, 0, 1, 0]]
-}
