module Day21 where

import qualified Data.Map as M
import Data.List.Split (splitOn)
import Debug.Trace (trace)

inputLines = fmap lines $ readFile "inputs/input21.txt"

-- Star 1

evaluate map key [num] = read num
evaluate map key [key1, "+", key2] = ((M.!) map key1) + ((M.!) map key2) 
evaluate map key [key1, "*", key2] = ((M.!) map key1) * ((M.!) map key2) 
evaluate map key [key1, "-", key2] = ((M.!) map key1) - ((M.!) map key2) 
evaluate map key [key1, "/", key2] = ((M.!) map key1) `Prelude.div` ((M.!) map key2) 


computeSolution1 inputLines =
    let splitLists = map (splitOn " ") inputLines
        parsedLists = [(take (length keyWithColon - 1) keyWithColon, expr) | (keyWithColon:expr) <- splitLists]
        mapEntry (key, expr) = evaluate calculationMap key expr
        calculationMap = M.fromList [(key, mapEntry (key, expr)) | (key, expr) <- parsedLists]
    in (M.!) calculationMap "root"


-- Star 2

data Symbolic = Num Int | Var String | Plus Symbolic Symbolic | Mult Symbolic Symbolic | Minus Symbolic Symbolic | Div Symbolic Symbolic deriving (Show)

plus (Num a) (Num b) = Num (a + b)
plus a@(Num _) (Plus b@(Num _) c) = plus (plus a b) c
plus a b = Plus a b

mult (Num a) (Num b) = Num (a * b)
mult a@(Num _) (Plus b c) = plus (mult a b) (mult a c)
mult a@(Num _) (Mult b@(Num _) c) = mult (mult a b) c
mult a@(Num _) (Minus b c) = minus (mult a b) (mult a c)
mult a b@(Num _) = mult b a
mult a b = Mult a b

minus (Num a) (Num b) = Num (a - b)
minus a b = Minus a b

div (Num a) (Num b) = Num (a `Prelude.div` b)
div a b = Div a b

-- evaluate' :: M.Map String Symbolic -> String -> [String] -> Symbolic
-- evaluate' map "root" [key1, _, key2] = trace ((show res1) ++ " == " ++ (show res2)) $ res1 `minus` res2
--     where (res1, res2) = (((M.!) map key1),  ((M.!) map key1))
-- evaluate' map "humn" _ = Var "humn"
-- evaluate' map key [num] = Num (read num)
-- evaluate' map key [key1, "+", key2] = ((M.!) map key1) `plus` ((M.!) map key2) 
-- evaluate' map key [key1, "*", key2] = ((M.!) map key1) `mult` ((M.!) map key2) 
-- evaluate' map key [key1, "-", key2] = ((M.!) map key1) `minus` ((M.!) map key2) 
-- evaluate' map key [key1, "/", key2] = ((M.!) map key1) `Day21.div` ((M.!) map key2) 

evaluate'' map "root" [key1, _, key2] = trace ((show res1) ++ " == " ++ (show res2)) $ res1 - res2
    where (res1, res2) = (((M.!) map key1),  ((M.!) map key2))
evaluate'' map key [num] = read num
evaluate'' map key [key1, "+", key2] = ((M.!) map key1) + ((M.!) map key2) 
evaluate'' map key [key1, "*", key2] = ((M.!) map key1) * ((M.!) map key2) 
evaluate'' map key [key1, "-", key2] = ((M.!) map key1) - ((M.!) map key2) 
evaluate'' map key [key1, "/", key2] = ((M.!) map key1) `Prelude.div` ((M.!) map key2) 

computeRootDifference inputLines val =
    let splitLists = map (splitOn " ") inputLines
        parsedLists = [(take (length keyWithColon - 1) keyWithColon, expr) | (keyWithColon:expr) <- splitLists, keyWithColon /= "humn:"] ++ [("humn", [show val])]
        mapEntry (key, expr) = evaluate'' calculationMap key expr
        calculationMap = M.fromList [(key, mapEntry (key, expr)) | (key, expr) <- parsedLists]
    in (M.!) calculationMap "root"

binarySearch :: (Int -> Int) -> Int -> Int -> Int
binarySearch function left right
    | left > right + 1 || right > left + 1 = -- assumption: function left < 0, function right > 0, f is continuous and has an integer root
        let middle = (left + right) `Prelude.div` 2
            middleVal = function middle
        in if (trace ((show middle) ++ ": " ++ show middleVal) middleVal) == 0 then middle else if middleVal < 0 then binarySearch function middle right else binarySearch function left middle
    | otherwise = 0

computeSolution2 inputLines = binarySearch (computeRootDifference inputLines) 83397964201949 0

-- Solutions (IO)
solution1 = fmap computeSolution1 inputLines
solution2 = fmap computeSolution2 inputLines
