import Data.List 
import MakeTotal

kw0 :: [Int]
kw0 = [3,106,82,42,50,23,35,106,138,134,142,146,154,55,59,218,230,107,123,127]

kw1 :: [Int]
-- kw1 = [42,50,23,35,138,134,142,146,154,55,59,218,230,107,123]
kw1 = [38,46,66,138,134,142,146,154,55,59,218,230,107,123]

sumTwo :: [Int] -> [Int]
sumTwo [] = []
sumTwo (x1:x2:xs) = (x1 + x2) : sumTwo xs
  
-- sort $ map sum $ subsequences kwSubset
-- nub $ sort $ map sumTwo $ permutations [15,19,23,27,31,35]

centsD :: Double -> Double
centsD d = log(d) / (log(2**(fromRational(1/1200))))


searchSubset1 :: Int -> [Int] -> [[Int]]
searchSubset1 n xs = [y | y <- subsequences xs, sum y == n]

-- mapM_ putStrLn $ map show $ searchSubset1 621 kw1


searchSubset2 :: Int -> [[Int]] -> [[Int]]
searchSubset2 n [] = []
searchSubset2 n (x:xs)
  | not (null $ searchSubset1 n x) = x : searchSubset2 n xs
  | otherwise                          = searchSubset2 n xs

removeElems :: [Int] -> [Int] -> [Int]
removeElems [] ys = ys
removeElems _ []   = []
removeElems (x:xs) ys = removeElems xs (filter (/= x) ys)

-- having defined the larger set
-- then remove those elements from the total set.
-- which gives the remainder set.

subsetRem_X :: Int -> Int -> [Int] -> [[Int]]
subsetRem_X a b ys =
  map (\x -> removeElems x ys) $
  searchSubset2 b $
  searchSubset1 a ys


included :: [Int] -> [Int] -> Bool
included [] _ = True
included (x:xs) ys =
  if elem x ys
  then included xs ys
  else False

-- map (\x -> included [134,142] x) subsetRem 828 426

including :: [Int] -> [[Int]] -> [[Int]]
including [] ys = []
including _ []   = []
including xs (y:ys)
  | included xs y = y : including xs ys
  | otherwise       = including xs ys

--------------------------------------------------------------
-- make total sets
mtSets' :: [Int] -> [Int] -> [[Int]]
mtSets' totalSet  xs
  | mod (sum totalSet) (sum xs) /= 0 = []
  | length xs /= 4 = []
  | otherwise = searchSubset2 b $ subsetRem_X c d totalSet
    where
      b = divisor * (xs !! 1)
      c = divisor * ((xs !! 2) + (xs !! 3))
      d = divisor * (xs !! 3)
      divisor = div (sum totalSet) (sum xs) 

mtSets :: [Int] -> Int -> [([Int], [[Int]])] 
mtSets totalSet num = zip (makeTotal num) (map (\x -> mtSets' totalSet x) $ makeTotal num) 

-- mtSetsDisplay totalSet num = map fst $ mtSets totalSet num 
mtSetsDisplay :: [Int] -> Int -> [([Int], [[Int]])]
mtSetsDisplay totalSet num =  mtSets totalSet num 
 
displayList :: [Int] -> Int -> [([Int], [[Int]])] -> [String]
displayList _ _ [] = []
displayList totalSet num (x:xs) = ((show (fst x) ) ++ "\t"++ (show (length (snd x)))++ "x:\t" ++ (show (map (divisor*) (fst x))) ++ "\t= " ++ (show (sum totalSet)) ++"\n\n")  : displayList totalSet num xs
  where
    divisor = div (sum totalSet) num

displaySets :: [Int] -> Int -> IO()
displaySets totalSet num = do
  let mtSD = mtSetsDisplay totalSet num
  putStrLn $ unwords $ displayList totalSet num mtSD
--------------------------------------------------------------------------------

getIntList :: IO [Int]
getIntList = do
  line <- getLine
  return (read line :: [Int]) 

getInt :: IO Int
getInt = do
  line <- getLine
  return (read line :: Int) 

main = do
  putStrLn ""
  putStr "kw0 = "
  print kw0
  putStrLn ""
  putStr "kw1 = "
  print kw1
  putStrLn ""
  putStrLn "enter the divisor: "
  num <- getInt
  putStrLn "enter the totalSet, 0 = kw0, 1 = kw1 :"
  totalSet <- getInt
  if totalSet == 0
    then displaySets kw0 num
    else  displaySets kw1 num
