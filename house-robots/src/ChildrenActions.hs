{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use null" #-}
{-# HLINT ignore "Redundant if" #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module ChildrenActions where

import EnvironmentFunction (isInRange,elementAt,updateBoard)
import Environment
import EnvironmentElements
import RobotActions

import GHC.Read (choose)

import Utils
import System.IO.Unsafe

childrenAction :: Int -> Int -> [Elements] -> [Elements]
childrenAction n m board =
    let children = getChildren board board []
    in childrenAction1 n m board children

childrenAction1 :: Int -> Int -> [Elements] -> [Elements] -> [Elements ]
childrenAction1 n m board [] = board
childrenAction1 n m board (Child (a,b) False : children) =
    let board1 = childAction n m a b board
    in childrenAction1 n m board1 children
childrenAction1 n m board (Child (a,b) True : children) = childrenAction1 n m board children


childAction :: Int -> Int -> Int -> Int -> [Elements] -> [Elements]
childAction n m posX posY board =
    if move == 1
        then let nPos = childMovePos n m posX posY board in doChildAction (posX,posY) nPos n m board
        else board
    where move = rand 0 1 ----

doChildAction :: (Int, Int) -> (Int, Int) -> Int -> Int -> [Elements] -> [Elements]
doChildAction (oldx,oldy) (nX,nY)n m board
  | nX == 1000 = board
  | otherwise = let (dx,dy) = getDirection (oldx,oldy) (nX,nY)
                   in moveChild child elemt (dx,dy) board n m
  where
      elemt = elementAt board (nX, nY)
      child = head (elementAt board (oldx,oldy))

moveChild :: Elements -> [Elements] -> (Int, Int) -> [Elements]->Int ->Int -> [Elements]
moveChild (Child (a,b) False) elemt (dx,dy) board n m=
    case elemt of
        [] -> leaveDirt board1 (a,b) n m
        _ -> if canMoveObstacle (Child(a,b) False) elemt (dx,dy) n m board
                then
                   let board2 = moveObstacle [Obstacle (a+dx,b+dy)] (dx,dy) board1
                    in leaveDirt board2 (a,b) n m
                else leaveDirt board (a,b) n m
    where newChild = Child (a+dx,b+dy) False
          board1 = updateBoard board (Child(a,b) False) newChild


leaveDirt :: [Elements] -> (Int,Int) -> Int -> Int -> [Elements]
leaveDirt board (a,b) n m =
    let step1 = get3x3Square board (a,b)
        step2 = chooseWhereDirt board step1 n m
        step3 = howMuchDirt board step1 step2
        step4 = placeDirt board step2 step3
    in step4


placeDirt :: [Elements] -> [(Int, Int)] -> Int -> [Elements]
placeDirt board free 0 = board

placeDirt board ((a,b):xs) c =
    let dirt = Dirt (a,b)
        board1 = board ++ [dirt]
    in placeDirt board1 xs (c-1)


howMuchDirt :: [Elements] -> [(Int, Int)] -> [(Int,Int)] -> Int
howMuchDirt board square [] = 0
howMuchDirt board square free =
    let kidsNo = countChildren board square 0
        maxDirt | kidsNo==1 = 1
                | kidsNo==3 = 3
                | otherwise = 6 
        freeNo = length free

    in if maxDirt > freeNo
        then rand 0 freeNo
        else rand 0 maxDirt

countChildren :: [Elements] -> [(Int, Int)] -> Int -> Int
countChildren board [] count = count
countChildren board (x:xs) count =
    let e = elementAt board x
    in auxCount e board xs count

auxCount :: [Elements] -> [Elements] -> [(Int, Int)] -> Int -> Int
auxCount [Child(a,b) False] board xs count = countChildren board xs (count+1)
auxCount _ board xs count = countChildren board xs count


chooseWhereDirt :: [Elements] -> [(Int,Int)] ->Int ->Int -> [(Int,Int)]
chooseWhereDirt board (x:xs) n m = do
    x <- xs
    let e = elementAt board x
      in case e of
        [] -> if uncurry isInRange x n m then r ++ [x] else r
        _ -> r
    where r = []


get3x3Square :: [Elements] -> (Int, Int) -> [(Int,Int)]
get3x3Square board (a,b) =
    let square = []
        sq1 = square ++ [(a,b)]
        sq2 = sq1 ++ [(a+1,b)]
        sq3 = sq2 ++ [(a-1,b)]
        sq4 = sq3 ++ [(a+1,b+1)]
        sq5 = sq4 ++ [(a+1,b-1)]
        sq6 = sq5 ++ [(a-1,b-1)]
        sq7 = sq6 ++ [(a-1,b+1)]
        sq8 = sq7 ++ [(a,b+1)]
        sq9 = sq8 ++ [(a,b-1)]
    in sq9

getDirection :: (Int,Int) -> (Int,Int) -> (Int,Int)
getDirection (oldx,oldy) (nX,nY)
  | oldx == nX && oldy > nY = (0,-1)
  | oldx == nX && oldy < nY = (0,1)
  | oldx > nX && oldy == nY = (-1,0)
  | oldx < nX && oldy == nY = (1,0)

moveObstacle :: [Elements] -> (Int,Int)-> [Elements] -> [Elements]
moveObstacle [] (dx,dy) board = board

moveObstacle [Obstacle(a,b)] (dx,dy) board =
    let e = elementAt board (a+dx,b+dy)
        board1 = updateBoard board (head[Obstacle (a,b)]) (head e)
    in if e == []
        then let board1 = updateBoard board (head[Obstacle (a,b)]) (Obstacle (a+dx,b+dy))
             in moveObstacle e (dx,dy) board1
        else let board1 = updateBoard board (head[Obstacle (a,b)]) (head e)
              in moveObstacle e (dx,dy) board1


canMoveObstacle :: Elements -> [Elements] -> (Int, Int) -> Int -> Int -> [Elements] -> Bool
canMoveObstacle _ [] (dx,dy) n m board = True

canMoveObstacle child [Obstacle (a,b)] (dx,dy) n m board =
     if isInRange (a+dx) (b+dy) n m
        then
            let elemt = elementAt board (a+dx,b+dy)
             in canMoveObstacle child elemt (dx,dy) n m board
        else False

canMoveObstacle child _ _ n m board = False


childMovePos :: Int -> Int -> Int -> Int -> [Elements] -> (Int, Int)
childMovePos n m posX posY board =
    let  x = Utils.rand1 1 4
         r = unsafePerformIO x
    in case r of
        1 -> if isInRange (posX-1) posY n m then getPosToMoveChild (posX-1) posY board else childMovePos n m posX posY board
        2 -> if isInRange (posX+1) posY n m then getPosToMoveChild (posX+1) posY board else childMovePos n m posX posY board
        3 -> if isInRange posX (posY-1) n m then getPosToMoveChild posX (posY-1) board else childMovePos n m posX posY board
        _ -> if isInRange posX (posY+1) n m then getPosToMoveChild posX (posY+1) board else childMovePos n m posX posY board



getPosToMoveChild :: Int -> Int -> [Elements] -> (Int, Int)
getPosToMoveChild x y board = if (elementAt board (x,y) == []) || (elementAt board (x,y) == [Obstacle (x,y)]) then (x,y) else (1000,1000)
