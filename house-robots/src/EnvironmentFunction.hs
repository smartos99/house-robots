{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module EnvironmentFunction(
    elementAt
    ,remove
    ,isInRange
    ,updateBoard
    ,rand
)where

import EnvironmentElements ( Elements, column, row ) 
import System.IO.Unsafe ( unsafePerformIO )

import Utils(rand,rand1)


elementAt :: [Elements ] -> (Int ,Int ) -> [Elements ]

elementAt l (a,b) = do
    x <- l
    if (let r = row x
            c = column x
        in a == r && b == c
        )
        then result ++ [x]
        else result
    where
        result = []

isInRange :: Int -> Int -> Int -> Int -> Bool
isInRange a b n m = do
    (b>=1 && a >= 1) && (b <= m && a<=n)
    

updateBoard :: [Elements] -> Elements -> Elements -> [Elements]
updateBoard xs oElem nElem = do
  nEnv ++ [nElem]
  where
    nEnv = remove oElem xs False

remove :: Elements -> [Elements] -> Bool ->[Elements]
remove _ [] _ = []
remove element (x:xs) False | x == element = remove element xs True 
                            | otherwise = x: remove element xs False 
remove element (x:xs) True = x : remove element xs True 
