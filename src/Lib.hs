
module Lib
    ( someFunc
    ) where

import Data.Array

data Cell = Definite Int | Unknown [Int]
type Board = Array (Int,Int) Cell


someFunc :: IO ()
someFunc = putStrLn "someFunc"

getCell :: Board -> (Int,Int)-> Cell
getCell board (x,y) = board ! (x,y)

emptyBoard :: Board
emptyBoard = array ((0,0),(8,8)) [((x,y),Unknown [1..9])| x<- [0..8], y<-[0..8]]

-- return the board with the cell at x,y  updated to be a definite Cell of number new
updateBoard :: Board -> Int -> Int -> Int -> Board
updateBoard board x y new = updateCells (board // [((x,y), Definite new)]) (x,y) new

--takes the cell position that has just been updated, then updates the possibilities of the rest of the board accordingly
updateCells :: Board -> (Int,Int) -> Int -> Board
updateCells board (x,y) newNum = do
                            --get all cells in the cross centered on (x,y), and in the correct block, call removePossibility on them, then replace them in board.
                            let updatedCells = board //  [(pos,(removePossibility (getCell board pos) newNum)) | pos <- horiz x y  ++ vert x y  ++ box x y]
                            cleanBoard updatedCells



horiz::Int->Int -> [(Int,Int)]                              
horiz x y = [(newX,y)|newX <- [0..8]]
vert::Int->Int -> [(Int,Int)]   
vert x y = [(x,newY)|newY <- [0..8]] 
box::Int->Int -> [(Int,Int)]   
box x y = [(newX,newY) |newX<- [(boxpoints !! x)..((boxpoints !! x)+2)],newY<- [(boxpoints !! y)..((boxpoints !! y)+2)]]
    where boxpoints = [0,0,0,3,3,3,6,6,6]


--iterates though the board and turns Unkowns of 1 item into Definites
cleanBoard :: Board -> Board
cleanBoard board = board // [((x,y),(cleanCell (getCell board (x,y))))|x<-[0..8],y<-[0..8]]
            where   cleanCell (Definite v) = Definite v
                    cleanCell (Unknown (v:[])) = Definite v -- TODO: if this gets called, we need to call updateCells centered on this cell again
                    cleanCell (Unknown (v:vs)) = Unknown (v:vs)



removePossibility :: Cell -> Int -> Cell
removePossibility (Unknown x) r = (Unknown (filter (/= r) x))
removePossibility (Definite x) r  = (Definite x)



prettyPrintBoard :: Maybe Board -> IO()
prettyPrintBoard Nothing = putStrLn "Failed to solve"
prettyPrintBoard (Just board) = putStrLn (concat [concat (["|" ++ getDefiniteValue (getCell board (x,y) ) | x <- [0..8]])++"\n-------------------\n"|y<-[0..8]])
                            where   getDefiniteValue (Definite x) = show x
                                    getDefiniteValue (Unknown _) = " "
                            
conflict :: Board -> (Int,Int) -> Bool
conflict board (8,_) = False
conflict board (_,8) = False
conflict board (x,y) = r (getCell board (x,y))                        
                        where 
                                r (Unknown _) = ((conflict board (x+1,y)) || (conflict board (x,y+1)))
                                r (Definite v) = foldr (||) False [((getDefValue (getCell board pos)) == v) | pos <- horiz x y ++ vert x y ++ box x y, pos /= (x,y)]
                                
                                getDefValue (Definite v) = v
                                getDefValue (Unknown _) = 0


solve :: Board -> Maybe Board
solve board  = do
                    --if board has conflicts, return Nothing
                    if conflict board (0,0) then Nothing
                    else do

                        let unsolvedCells = [((x,y),(getCell board (x,y))) | x<-[0..8],y<-[0..8] , getDefValue (getCell board (x,y))==0 ]
                        if (length unsolvedCells == 0) then Just board
                        else do
                    --order unsolved cells by how many possibilities
                        let mostConstrained = foldr (compareConstraint) (head unsolvedCells) unsolvedCells


                    --iterte through unsolved cells
                        if length (getPossibilities (snd mostConstrained)) == 0 then Nothing
                        --if head has 1 possibility, set it to a definite, call updateCells, call solve
                        else if length (getPossibilities (snd mostConstrained)) == 1 then 
                            solve (updateBoard board (fst(fst mostConstrained)) (snd(fst mostConstrained)) (head(getPossibilities(snd mostConstrained))))                    
                    --else get the boards where it is each of its possibilities, call solve on these
                        else head (cleanNothings [solve (updateBoard board (fst(fst mostConstrained)) (snd(fst mostConstrained)) v) | v <-  getPossibilities (snd mostConstrained) ])
                        


                    where 
                        getDefValue (Definite v) = v
                        getDefValue (Unknown _) = 0

                        compareConstraint ((x1,y1),(Unknown v1)) ((x2,y2),(Unknown v2))     | length v1 > length v2 = ((x2,y2),(Unknown v2))
                                                                                            | otherwise = ((x1,y1),(Unknown v1))
                        getPossibilities (Unknown v) = v

                        cleanNothings [] = []
                        cleanNothings (Nothing:xs) = cleanNothings xs
                        cleanNothings ((Just x):xs)= (Just x): (cleanNothings xs)

start :: [Int] -> IO ()
start v = do 
                    let board = parse v (0,0) emptyBoard
                    let solved = solve board
                    prettyPrintBoard solved



parse:: [Int]->(Int,Int) -> Board -> Board
parse (v:vs) (8,y) board    | v /= 0  = parse vs (0,y+1) (updateBoard board 8 y v)
                            | otherwise = parse vs (0, y+1) board
parse (v:vs) (x,y) board    | v /= 0  = parse vs (x+1,y) (updateBoard board x y v)
                            | otherwise = parse vs (x+1,y) board
parse [] (x,y) board      =   board