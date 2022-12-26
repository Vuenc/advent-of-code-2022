module Day20 where

import qualified Data.Map as M
import Data.Ratio
import Data.Bool (bool)
import Data.List (find)
import Data.Maybe (fromJust)
import GHC.Float (rationalToFloat, fromRat)
-- import Debug.Trace (traceShow, trace)
trace a b = b -- disabling the debug printing

inputLines = fmap lines $ readFile "inputs/input20.txt"

-- Star 1

-- data DoublyLinkedList a = Head {
--     headNode :: Node a
-- }

-- data Node a = HeadNode {
--     _start :: Node a,
--     _end :: Node a
-- } | Node {
--     _value :: a,
--     _next :: Node a,
--     _prev :: Node a
-- }

-- -- partially implemented, for convenience
-- value (Node {_value=_value}) = _value

-- next (Node {_next=_next}) = _next
-- next (HeadNode {_start=_start}) = _start

-- previous (Node {_prev=_prev}) = _prev
-- previous (HeadNode {_end=_end}) = _end

-- isHead (Node {}) = False
-- isHead (HeadNode {}) = True

-- empty = HeadNode {_start=empty, _end=empty}


-- insertNodeBefore newNode currentNode@(Node {_prev=_prev, _next=_next}) =
--     let beforeNewNode = insertNodeAfter newNode _prev
--         newCurrentNode = currentNode {_prev=newNode, _next=newAfterCurrentNode}
--         newAfterCurrentNode = insertNodeBefore newCurrentNode _next
--     in newCurrentNode
-- insertNodeBefore newNode currentNode@(HeadNode {_end=_prev, _start=_next}) =
--     let beforeNewNode = insertNodeAfter newNode _prev
--         newCurrentNode = currentNode {_end=newNode, _start=newAfterCurrentNode}
--         newAfterCurrentNode = insertNodeBefore newCurrentNode _next
--     in newCurrentNode

-- insertNodeAfter newNode currentNode@(Node {_prev=_prev, _next=_next}) =
--     let afterNewNode = insertNodeBefore newNode _next
--         newCurrentNode = currentNode {_prev=newBeforeCurrentNode, _next=newNode}
--         newBeforeCurrentNode = insertNodeAfter newCurrentNode _prev
--     in newCurrentNode
-- insertNodeAfter newNode currentNode@(HeadNode {_end=_prev, _start=_next}) =
--     let afterNewNode = insertNodeBefore newNode _next
--         newCurrentNode = currentNode {_end=newBeforeCurrentNode, _start=newNode}
--         newBeforeCurrentNode = insertNodeAfter newCurrentNode _prev
--     in newCurrentNode

-- insertBefore value currentNode =
--     let newNode = Node {_prev=previous currentNode, _next=currentNode, _value=value}
--     in insertNodeBefore newNode currentNode

-- insertAfter value currentNode =
--     let newNode = Node {_prev=currentNode, _next=next currentNode, _value=value}
--     in insertNodeAfter newNode currentNode

-- -- delete currentNode@(Node {_prev=_prev, _next=_next}) =

-- toList node =
--     let previousPart = takeWhile (not . isHead) $ (iterate previous) node
--         nextPart = takeWhile (not . isHead) $ (iterate next) node
--         selfValue = if not $ isHead node then [value node] else []
--     in (map value previousPart) ++ selfValue ++ (map value nextPart)

-- fromList l = foldr insertAfter empty l

-- insertBefore val currentNode@(Node {_prev=_prev, _next=_next}) =
--     let newNode = Node {_prev=beforeNewNode, _next=afterNewNode, _value=val}
--         beforeNewNode = insertNodeAfter newNode _prev
--         newCurrentNode = currentNode {_prev=newNode, _next=newAfterCurrentNode}
--         newAfterCurrentNode = insertNodeBefore newCurrentNode _next
--     in newCurrentNode

data MovableList a = MList {
    entries :: M.Map (Rational) a,
    -- current :: Ratio Int,
    minVal :: Rational,
    maxVal :: Rational,
    keysByOriginalOrder :: M.Map Int (Rational)
} deriving (Show)

-- empty = MList { entries = M.fromList [(i % 1, e) | i <- [0..], e <- ]}

fromList l@(_:_) = MList { entries = M.fromList [(i % 1, e) | (i, e) <- zip [0..] l], minVal = (0 % 1), maxVal = ((fromIntegral $ length l - 1) % 1), keysByOriginalOrder=M.fromList $ take (length l) $ zip [0..] (map (% 1) [0..])}
-- , current = (0 % 1), 

toList (MList {entries=entries}) = map snd $ M.toList entries

lookLeft (MList {entries=entries, maxVal=maxVal}) current =
    case M.lookupLT current entries of
        Just (left, _) -> left
        Nothing -> maxVal

lookRight (MList {entries=entries, minVal=minVal}) current =
    case M.lookupGT current entries of
        Just (right, _) -> right
        Nothing -> minVal

insertBefore mlist@(MList {entries=entries, minVal=minVal}) current value
    | current == minVal = (mlist {entries=M.insert (minVal - 1) value entries, minVal=(minVal - 1)}, minVal - 1)
    | otherwise = 
        let leftOfCurrent = lookLeft mlist current
            newKey = ((current + leftOfCurrent) / 2)
        in (mlist {entries=M.insert newKey value entries}, newKey)

insertAfter mlist@(MList {entries=entries, maxVal=maxVal}) current value
    | current == maxVal = (mlist {entries=M.insert (maxVal + 1) value entries, maxVal=(maxVal + 1)}, (maxVal + 1))
    | otherwise = 
        let rightOfCurrent = lookRight mlist current
            newKey = ((current + rightOfCurrent) / 2)
        in (mlist {entries=M.insert newKey value entries}, newKey)

delete mlist@(MList {entries=entries, minVal=minVal, maxVal=maxVal}) index
    | index == minVal =  mlist {entries=M.delete index entries, minVal=lookRight mlist index}
    | index == maxVal =  mlist {entries=M.delete index entries, maxVal=lookLeft mlist index}
    | otherwise = mlist {entries=M.delete index entries}

-- moveRightBy mlist current = 

-- moveNumber :: (MovableList Int, Int, c) -> (MovableList Int, Int, c)
moveNumber (mlist, currentIndex, movedNumbers) =
    let key = ((M.!) (keysByOriginalOrder mlist) currentIndex)
        (val) = (M.!) (entries mlist) key
        mlistWithoutCurrent = delete mlist key
        (movedMlist, newKey)
            -- | hasMoved = (mlist, key)
            | val == 0 = (mlist {entries=M.insert key (val) (entries mlist)}, key)
            | val > 0 =
                let beforeNewInsert = (iterate (lookRight mlistWithoutCurrent) key) !! (val `mod` 4999)
                in insertAfter mlistWithoutCurrent beforeNewInsert (val)
            | val < 0 =
                let afterNewInsert = (iterate (lookLeft mlistWithoutCurrent) key) !! ((-val) `mod` 4999)
                in insertBefore mlistWithoutCurrent afterNewInsert (val)
        newMlist = movedMlist {keysByOriginalOrder=M.insert currentIndex newKey $ keysByOriginalOrder movedMlist}
        newMovedNumbers
            -- | hasMoved = movedNumbers
            | otherwise = movedNumbers + 1
    in trace (show currentIndex) $ trace ("    min/max:" ++ show (minVal mlist, maxVal mlist) ++ " / " ++ show (fromRational $ toRational $ minVal mlist, fromRational $ toRational $ maxVal mlist)) $ trace ("    key: " ++ show key ++ " / " ++ show (fromRational $ toRational key)) $ trace ( " -> " ++ (show newKey) ++ " / " ++ show (fromRational $ toRational $ newKey)) (newMlist, currentIndex + 1, newMovedNumbers)

-- showResult result = traceShow ((show $ map snd . toList $ (\(x,_,_) -> x) $ result) ++ ", " ++ (show $ (\(_,x,_) -> x) result) ++ ", " ++ (show $ (\(_,_,x) -> x) result)) $ traceShow (show $ M.toList $ entries $ (\(x,_,_) -> x) $ result) result

-- moveNumber' = Day20.showResult . moveNumber

computeSolution1 inputLines =
    let mlist = fromList $ map (read) inputLines :: MovableList (Int)
        (movedList, _, _) = head $ dropWhile (\(_, _, movedNumbers) -> movedNumbers < length inputLines) $ drop 1 $ iterate moveNumber (mlist, 0, 0)
        indexOf0 = fst $ fromJust $ find (\(_, (val)) -> val == 0) $ M.toList $ entries movedList
        index1000 = (iterate (lookRight movedList) indexOf0) !! 1000
        index2000 = (iterate (lookRight movedList) index1000) !! 1000
        index3000 = (iterate (lookRight movedList) index2000) !! 1000
        result = [(M.!) (entries movedList) index | index <- [index1000, index2000, index3000]]
    in sum result

l = lines "1\n2\n-3\n3\n-2\n0\n4"

-- Star 2


computeSolution2 inputLines =
    let decryptionKey = 811589153
        mlist = fromList $ map ((*decryptionKey) . read) inputLines :: MovableList (Int)
        mixList mlist = (\(x,_,_) -> x) $ head $ drop 5000 $ iterate moveNumber (mlist, 0, 0)
        movedList = (iterate mixList mlist) !! 10
        indexOf0 = fst $ fromJust $ find (\(_, (val)) -> val == 0) $ M.toList $ entries movedList
        index1000 = (iterate (lookRight movedList) indexOf0) !! 1000
        index2000 = (iterate (lookRight movedList) index1000) !! 1000
        index3000 = (iterate (lookRight movedList) index2000) !! 1000
        result = [(M.!) (entries movedList) index | index <- [index1000, index2000, index3000]]
    in sum result


-- Solutions (IO)
solution1 = fmap computeSolution1 inputLines
solution2 = fmap computeSolution2 inputLines
