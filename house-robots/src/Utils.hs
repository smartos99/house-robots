{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Utils  where
import EnvironmentElements 
import Data.Matrix 

import System.IO.Unsafe ( unsafePerformIO )
import System.Random

rand :: Int -> Int -> Int 
rand min max = unsafePerformIO (getStdRandom(randomR(min,max)))

rand1 :: Int -> Int -> IO Int 
rand1 min max = do randomRIO (min,max :: Int)

printBoard :: [Elements] -> Matrix String  -> Matrix String
printBoard [] mat = mat
printBoard (Child (a,b) c:board) mat =
    if mat ! (a,b) == "##"
        then let nmat = setElem "C" (a,b) mat
             in printBoard board nmat
        else 
            let elemt = mat ! (a,b)
                nelemt = elemt ++ "C"
                nmat = setElem nelemt (a,b) mat
            in printBoard board  nmat



printBoard (Obstacle (a,b):board) mat =
    if mat ! (a,b) == "##"
        then let nmat = setElem "O" (a,b) mat
             in printBoard board nmat
        else 
            let elemt = mat ! (a,b)
                nelemt = elemt ++ "O"
                nmat = setElem nelemt (a,b) mat
            in printBoard board  nmat

printBoard (Playpen (a,b):board) mat =
    if mat ! (a,b) == "##"
        then let nmat = setElem "P" (a,b) mat
             in printBoard board nmat
        else 
            let elemt = mat ! (a,b)
                nelemt = elemt ++ "P"
                nmat = setElem nelemt (a,b) mat
            in printBoard board  nmat

printBoard (Robot (a,b) c:board) mat =
    if mat ! (a,b) == "##"
        then let nmat = setElem "R" (a,b) mat
             in printBoard board nmat
        else 
            let elemt = mat ! (a,b)
                nelemt = elemt ++ "R"
                nmat = setElem nelemt (a,b) mat
            in printBoard board  nmat

printBoard (Dirt (a,b):board) mat =
    if mat ! (a,b) == "##"
        then let nmat = setElem "D" (a,b) mat
             in printBoard board nmat
        else 
            let elemt = mat ! (a,b)
                nelemt = elemt ++ "D"
                nmat = setElem nelemt (a,b) mat
            in printBoard board  nmat

printOutput :: Int -> Int -> [Elements] -> Bool -> Int -> Matrix String -> String ->IO()
printOutput n m board vBool time mat cleanPercentage = do
    print "_____________House-Robot_____________"
    print ("_____________clean"++show cleanPercentage++"_____________")
    print ("_____________Tiempo :" ++ show time ++"_____________")
    print ("_____________Variacion Aleatoria " ++ show vBool ++"_____________")
    print mat
