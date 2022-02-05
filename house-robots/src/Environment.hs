{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant if" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Environment(
    initiate
    ,isNotUsed
) where
import EnvironmentElements
    ( Elements(Dirt, Robot, Child, Obstacle,Playpen), column, row )
import EnvironmentFunction (isInRange,elementAt)

import Utils ( rand, rand1 )
import System.IO.Unsafe
import System.Random (randomRIO)


initiate :: Int -> Int -> Int -> Int -> Int -> Int -> [Elements]
initiate n m rNo cNo oNo dNo =
    let board = []
        playpen = board ++ putPlaypen n m cNo
        robots = putRobots n m rNo playpen
        children = putChildren n m cNo robots
        obstacles = putObstacles n m oNo children
        dirt = putDirt n m dNo obstacles
    in dirt


putPlaypen :: Int -> Int -> Int -> [Elements]
putPlaypen n m c = if isInRange rr rc n m 
    then putPlaypen1 n m rr rc (c-1) [Playpen (rr,rc)]
    else putPlaypen n m c
     where rr = rand 1 n 
           rc = rand 1 m
 

putPlaypen1 :: Int -> Int -> Int -> Int -> Int  -> [Elements] -> [Elements]
putPlaypen1 n m lr lc 0 env = env
putPlaypen1 n m lr lc c env =
    let w = rand1 1 4
        r = unsafePerformIO w
    in case r of
        1 -> if isInRange (lr-1) lc n m && isNotUsed (lr-1,lc) env then (let env1 = env ++[Playpen (lr-1,lc)] in putPlaypen1 n m (lr-1) lc (c-1) env1) else putPlaypen1 n m lr lc c env
        2 -> if isInRange (lr+1) lc n m && isNotUsed (lr+1,lc) env then (let env1 = env ++[Playpen (lr+1,lc)] in putPlaypen1 n m (lr+1) lc (c-1) env1) else putPlaypen1 n m lr lc c env
        3 -> if isInRange lr (lc-1) n m && isNotUsed (lr,lc-1) env then (let env1 = env ++[Playpen (lr,lc-1)] in putPlaypen1 n m lr (lc-1) (c-1) env1) else putPlaypen1 n m lr lc c env
        _ -> if isInRange lr (lc+1) n m && isNotUsed (lr,lc+1) env then (let env1 = env ++[Playpen (lr,lc+1)] in putPlaypen1 n m lr (lc+1) (c-1) env1) else putPlaypen1 n m lr lc c env
                    

putRobots :: Int -> Int -> Int -> [Elements] -> [Elements]
putRobots n m 0 board = board
putRobots n m rNo board =
    let pos = getPos n m board
        e = Robot pos False
    in putRobots n m (rNo-1) (board ++ [e])


putChildren :: Int -> Int -> Int -> [Elements] -> [Elements ]
putChildren n m 0 board = board
putChildren n m cNo board =
    let pos = getPos n m board
        e = Child pos False
    in putChildren n m (cNo-1) (board ++ [e])



putObstacles :: Int -> Int -> Int -> [Elements] -> [Elements]
putObstacles n m 0 board = board
putObstacles n m oNo board =
    let pos = getPos n m board
        e = Obstacle pos
    in putObstacles n m (oNo-1) (board ++ [e])

putDirt :: Int -> Int -> Int -> [Elements] -> [Elements]
putDirt n m 0 board = board
--putDirt n m dNo board =
--    if isInRange x y n m && isNotUsed (x,y) board
--        then putDirt n m (dNo-1) (board ++ [Dirt (x,y)])
--        else putDirt n m dNo board 
--    where x = rand 1 n
--          y = rand 1 m
    
    
putDirt n m dNo board = let pos = getPos n m board
                            e = Dirt pos
                        in putDirt n m (dNo-1) (board ++ [e])






getPos :: Int -> Int -> [Elements] -> (Int, Int)
getPos n m board = do
    if isInRange x y n m && isNotUsed (x,y) board
        then (x,y)
        else getPos n m board
    where
        x = rand 1 n
        y = rand 1 m

isNotUsed :: (Int, Int) -> [Elements] -> Bool
isNotUsed (a,b) board = 
    case xs of
        [] -> True
        _ -> False  
    where xs = elementAt board (a,b)



