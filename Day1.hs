module Day1 where

import Data.List
import Data.Function

inputLines :: IO [String]
inputLines = fmap lines $ readFile "inputs/input1.txt"

blocks :: IO [[Integer]]
blocks = fmap (\x -> tail $ foldr blocksFold [[]] x) inputLines
    where blocksFold "" (bs) = ([]:bs)
          blocksFold l (b:bs) = (((read l :: Integer):b):bs)

solution1 = fmap (\blocks -> maximum $ map sum blocks) blocks

best3 :: [[Integer]] -> Integer
best3 blocks = sum $ take 3 (sortBy (compare `on` (* (-1))) (map sum blocks))

solution2 = fmap best3 blocks