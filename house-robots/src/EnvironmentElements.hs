module EnvironmentElements where
import Control.Concurrent (yield)
    
data Elements = 
    Obstacle (Int,Int) 
    | Dirt (Int,Int)
    | Playpen (Int,Int)
    | Child (Int,Int) Bool 
    | Robot (Int,Int) Bool 
    deriving (Eq,Show)

column :: Elements -> Int 
column (Obstacle (x,y)) = y
column (Dirt (x,y)) = y
column (Playpen (x,y)) = y
column (Child (x,y) a) = y
column (Robot (x,y) a) = y


row :: Elements -> Int 
row (Obstacle (x,y)) = x
row (Dirt (x,y)) = x
row (Playpen (x,y)) = x
row (Child (x,y) a) = x
row (Robot (x,y) a) = x


