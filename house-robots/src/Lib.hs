module Lib
    ( simulation
    ) where

import Environment
import EnvironmentElements
import ChildrenActions
import EnvironmentFunction
import RobotActions
import Data.Matrix
import Utils



simulation :: IO ()
simulation = do
    let board = initiate 6 7 1 3 3 21
      in runSimulation board 6 7 4 0 False False
    
    let board = initiate 6 7 2 3 3 21
     in runSimulation board 6 7 4 0 False False

    let board = initiate 8 10 4 4 7 40
     in runSimulation board 8 10 4 0 False False

runSimulation :: [Elements] -> Int -> Int -> Int -> Int -> Bool -> Bool -> IO()
runSimulation board _ _ _ _ _ True = putStrLn "Simulation Ended"
runSimulation board n m t time vBool endBool = do
    let iMatrix = matrix n m $ \(i, j) -> "##"
        vMatrix =  printBoard board iMatrix
        dirtNo = length (getAllDirt board [])
        totalCells = (n*m)
        dirtPercent = div (dirtNo * 100) totalCells
        cleanPercentage = 100 - dirtPercent
     in printOutput n m board vBool time vMatrix (show cleanPercentage)



    let dirtNo = length (getAllDirt board [])
        totalCells = (n*m)
        dirtPercent = div (dirtNo * 100) totalCells
        ntime = time+1
        vBool = (ntime `mod` t == 0)

     in if dirtPercent < 40
         then runSimulation board n m t time vBool True

         else if vBool
             then let board1 = childrenAction n m board
                      board2 = rActions n m board1
                  in runSimulation board2 n m t ntime vBool endBool
             else let board3 = rActions n m board
                  in runSimulation board3 n m t ntime vBool endBool


