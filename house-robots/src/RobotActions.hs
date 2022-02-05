{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module RobotActions where

import EnvironmentElements
import EnvironmentFunction
import Environment
import Data.Matrix

rActions :: Int -> Int -> [Elements] -> [Elements]
rActions n m board = 
    let robots = getRobot board []  
    in rAction1 n m board robots

rAction1 :: Int -> Int -> [Elements] -> [Elements] -> [Elements]
rAction1 n m board [] = board
rAction1 n m board ((Robot (x,y) z):xs) =
    let board1 = robotActions board n m (Robot (x,y) z)
    in rAction1 n m board1 xs


robotActions :: [Elements] -> Int -> Int -> Elements -> [Elements]
robotActions board n m (Robot (x,y) z)=
    let 
        e = elementAt board (x,y)
        cell = remove (Robot (x,y) z) e False
    in doRobotActions board (Robot (x,y) z) cell n m

doRobotActions :: [Elements] -> Elements -> [Elements] -> Int -> Int -> [Elements]
doRobotActions board (Robot (x,y) False) [Dirt (a,b)] n m =
    clean board (Robot (x,y) False) (Dirt (a,b))

doRobotActions board (Robot (x,y) True) [Playpen (a,b),Child (c,d) True ] n m =
    leaveChild board (Robot (x,y) True) (Child (c,d) True)

doRobotActions board (Robot (x,y) True) [Child (a,b) True,Playpen (c,d)] n m =
    leaveChild board (Robot (x,y) True) (Child (a,b) True)

doRobotActions board (Robot (x,y) True) _ n m =
    let fmatrix = bfs board (x,y) n m
        playpen = getFullPlaypen board board []
        (ppx,ppy) = getNearestPP playpen fmatrix(n*m) (x,y)
        distance = fmatrix ! (ppx,ppy)
        (newr,newc) = getPath2 (ppx,ppy) fmatrix distance
        board1 = updateBoard board (Robot (x,y) True) (Robot (newr,newc) True)
        board2 = updateBoard board1 (Child (x,y) True) (Child (newr,newc) True)
    in board2

doRobotActions board (Robot (x,y) False) _ n m =
    let fmatrix = bfs board (x,y) n m
        dirt = getAllDirt board []
        (ddr,ddc) = getNearestDirt dirt fmatrix (n*m) (x,y)
        distance = fmatrix ! (ddr,ddc)
        pp = getFullPlaypen board board []
        (newr,newc) = if null pp
                       then getPath (ddr,ddc) fmatrix distance
                       else
                           let children = getChildren board board []
                           in if null children
                               then getPath (ddr,ddc) fmatrix distance
                               else
                                   let (childr,childc) = getNearestChild children fmatrix (n*m) (x,y)
                                       distChild = fmatrix !(childr,childc)
                                       (ppr,ppc) = getNearestPP pp fmatrix (n*m) (x,y)
                                   in if distance >= distChild && (ppr,ppc) /= (x,y)
                                       then getPath (childr,childc) fmatrix distChild
                                       else getPath (ddr,ddc) fmatrix distance
        nowCell = elementAt board (newr,newc)
        board1 = case nowCell of
            [Child(newr,newc) False] -> let board2 = updateBoard board (Robot(x,y) False) (Robot(newr,newc) True) in updateBoard board2 (Child(newr,newc) False) (Child(newr,newc) True)
            _ -> updateBoard board (Robot(x,y) False) (Robot(newr,newc) False)
    in board1

doRobotActions board _ _ _ _ = board

getRobot :: [Elements] -> [Elements] -> [Elements]
getRobot [] rBoard = rBoard
getRobot (Robot (x,y) z:board) rBoard = let rBoard1 = rBoard ++ [Robot (x,y) z] in getRobot board rBoard1
getRobot (_ : board) rBoard = getRobot board rBoard

getAllDirt :: [Elements] -> [Elements] -> [Elements]
getAllDirt [] rBoard = rBoard
getAllDirt (Dirt (x,y):board) rBoard = let rBoard1 = rBoard ++ [Dirt (x,y)] in getAllDirt board rBoard1
getAllDirt (_ : board) rBoard = getAllDirt board rBoard

getFullPlaypen :: [Elements] -> [Elements] -> [Elements] -> [Elements]
getFullPlaypen auxBoard [] result = result
getFullPlaypen auxBoard (Playpen(a,b):board) result =
    if let aux = remove (Playpen (a,b)) auxBoard False
        in isNotUsed (a,b) aux
        then let result1 = result ++ [Playpen (a,b)]
             in getFullPlaypen auxBoard board result1
        else  getFullPlaypen auxBoard board result

getFullPlaypen auxBoard (_:board) result = getFullPlaypen auxBoard board result

getChildren :: [Elements] -> [Elements] -> [Elements] -> [Elements]
getChildren auxBoard [] result = result
getChildren auxBoard ((Child(a,b) False):board) result =
    if let aux = remove (Child (a,b) False) auxBoard False
        in isNotUsed (a,b) aux
        then let result1 = result ++ [Child (a,b) False]
             in getChildren auxBoard board result1
        else  getChildren auxBoard board result

getChildren auxBoard (_:board) result = getChildren auxBoard board result


getNearestPP :: [Elements] -> Matrix Int -> Int -> (Int, Int) -> (Int, Int)
getNearestPP [] _ _ (x,y) = (x,y)
getNearestPP (Playpen (a,b):playpen) fMatrix minW (r,c) =
    let d = fMatrix ! (a,b)
    in if d > 0 && d < minW
        then getNearestPP playpen fMatrix d (a,b)
        else getNearestPP playpen fMatrix minW (r,c)

getNearestPP _ _ _ (r,c) = (r,c)

getNearestDirt :: [Elements] -> Matrix Int -> Int -> (Int, Int) -> (Int, Int)
getNearestDirt [] _ _ (x,y) = (x,y)
getNearestDirt (Dirt (a,b):dirt) fMatrix minW (r,c) =
    let d = fMatrix ! (a,b)
    in if d > 0 && d < minW
        then getNearestDirt dirt fMatrix d (a,b)
        else getNearestDirt dirt fMatrix minW (r,c)

getNearestDirt _ _ _ (r,c) = (r,c)

getNearestChild :: [Elements] -> Matrix Int -> Int -> (Int, Int) -> (Int, Int)
getNearestChild [] _ _ (x,y) = (x,y)
getNearestChild ((Child (a,b) False):children) fMatrix minW (r,c) =
    let d = fMatrix ! (a,b)
    in if d > 0 && d < minW
        then getNearestChild children fMatrix d (a,b)
        else getNearestChild children fMatrix minW (r,c)

getNearestChild _ _ _ (r,c) = (r,c)

clean :: [Elements] -> Elements -> Elements -> [Elements]
clean board (Robot (x,y) False) (Dirt (a,b)) = remove (Dirt (a,b)) board False
clean board _ _ = board




leaveChild :: [Elements] -> Elements -> Elements -> [Elements]
leaveChild board (Robot (x,y) True) (Child (a,b) True) =
    let board1 = updateBoard board (Robot (x,y) True) (Robot (x,y) False)
        board2 = updateBoard board1 (Child (a,b) True) (Child (a,b) False)
    in board2

bfs :: [Elements] -> (Int, Int) -> Int -> Int -> Matrix Int
bfs board (r,c) n m =
    let iMatrix =  matrix n m $ \(i, j) -> (-1)
        fMatrix = setElem 0 (r,c) iMatrix
        iPos = givePositions [(r,c)] fMatrix
    in bfs1 board fMatrix iPos 1

bfs1 :: [Elements] -> Matrix Int -> [(Int, Int)] -> Int -> Matrix Int
bfs1 board fMatrix [] _ = fMatrix
bfs1 board fMatrix pos width =
    let (fPos,nfMatrix) = bfsAux board fMatrix pos width []
        nPos = givePositions fPos nfMatrix
    in bfs1 board nfMatrix nPos (width + 1)

bfsAux :: [Elements] -> Matrix Int -> [(Int, Int)] -> Int -> [(Int, Int)] -> ([(Int, Int)], Matrix Int)
bfsAux _ fMatrix [] _ fPos = (fPos,fMatrix)
bfsAux board fMatrix ((a,b):pos) width fPos =
    let cell = elementAt board (a,b)
        (nfMatrix,boolean) = case cell of
            [] -> (setElem width (a,b) fMatrix ,True)
            [Dirt (a, b)] -> (setElem width (a, b) fMatrix, True)
            [Child (a, b) False] -> (setElem width (a, b) fMatrix, True)
            [Playpen (a, b)] -> (setElem width (a, b) fMatrix, True)
            _ -> (setElem (-2) (a, b) fMatrix, False)
    in if boolean then bfsAux board nfMatrix pos width (fPos ++ [(a, b)]) else bfsAux board nfMatrix pos width fPos

givePositions :: [(Int, Int)] -> Matrix Int -> [(Int, Int)]
givePositions oPos fMatrix = do
  (x, y) <- oPos
  (a, b) <- [(x + 1, y), (x -1, y), (x, y + 1), (x, y -1)]
  if isInRange a b (nrows fMatrix) (ncols fMatrix) && fMatrix ! (a, b) == -1 && notElem (a, b) newPos
    then newPos ++ [(a, b)]
    else newPos
  where
    newPos = []

minNeighbor :: [(Int, Int)] -> Matrix Int -> Int -> (Int, Int) -> (Int, Int)
minNeighbor [] _ _ (r, c) = (r, c)
minNeighbor ((a, b) : pos) fMatrix minW (r, c) =
  if isInRange a b (nrows fMatrix) (ncols fMatrix)
    then
      let dist = fMatrix ! (a, b)
       in if dist > -1 && dist < minW
            then minNeighbor pos fMatrix dist (a, b)
            else minNeighbor pos fMatrix minW (r, c)
    else minNeighbor pos fMatrix minW (r, c)

getPath :: (Int, Int) -> Matrix Int -> Int -> (Int, Int)
getPath (a, b) _ 1 = (a, b)
getPath (a, b) fMatrix d =
  let (r, c) = minNeighbor [(a + 1, b), (a -1, b), (a, b + 1), (a, b -1)] fMatrix d (a, b)
      nDist = fMatrix ! (r, c)
   in getPath (r, c) fMatrix nDist

getPath2 :: (Int, Int) -> Matrix Int -> Int -> (Int, Int)
getPath2 (a, b) _ 1 = (a, b)
getPath2 (a, b) _ 2 = (a, b)
getPath2 (a, b) fMatrix d =
  let (r, c) = minNeighbor [(a + 1, b), (a -1, b), (a, b + 1), (a, b -1)] fMatrix d (a, b)
      nDist = fMatrix ! (r, c)
   in getPath2 (r, c) fMatrix nDist