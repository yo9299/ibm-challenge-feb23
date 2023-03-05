
import qualified Data.List as L
import qualified Data.Array as A
import qualified Data.IntMap.Strict as M
import Data.Maybe
import PosList
import King
import Debug.Trace

type Var = Int


updateBoard :: Position -> Board -> Board
updateBoard pos board = foldr f board pl
    where pl = getPositions $ threatenPosition pos
          f p b = M.adjust (+1) (posToKey p) b

threatenPosition :: Position -> PosList
threatenPosition (i,j) = mkPosList $ [(i,j)] ++ col  ++ row ++ diagSE ++ diagNE ++ diagNW ++ diagSW
    where col = [(k,j) | k <- [1..n], k /= i]
          row = [(i,k) | k <- [1..n], k /= j]
          diagSE = [(i+l, j-l) | l <- [1..min (n-i) (j-1)]]
          diagNE = [(i+l, j+l) | l <- [1..min (n-i) (n-j)]]
          diagNW = [(i-l, j+l) | l <- [1..min (i-1) (n-j)]]
          diagSW = [(i-l, j-l) | l <- [1..min (i-1) (j-1)]]


selectPos' :: Domain -> Board -> Position
selectPos' dom b = L.maximumBy f $ getPositions dom
    where posVal :: Position -> Int
          posVal pos = length $ safeSpaces $ updateBoard pos b
          f pos1 pos2 = posVal pos1 `compare` posVal pos2

algo' :: Int -> Domain -> Board -> Maybe ([Position] , Board)
algo' k dom b
    | M.null dom = Nothing
    | k==1 = let newPos = selectPos' dom b
                 spaces = safeSpaces $ updateBoard newPos b
                 sols = kAlgo n spaces []
             in trace ("found queen assignment with " ++ show (length spaces) ++ " safe spaces. kAlgo returned: " ++ show sols) $
                if length spaces >= 20 
                   && isJust sols 
                   && (trace (show $ length $ fromJust sols) $ length (fromJust sols) == 48) then Just ([newPos], updateBoard newPos b)
                    else algo' k (M.delete (posToKey newPos) dom) b
    | otherwise = let chosenPos = selectPos dom
                      dom' = dom M.\\ threatenPosition chosenPos
                      b' = updateBoard chosenPos b
                  in case algo' (k-1) dom' b' of
                        Just (result, finalBoard) -> Just ((chosenPos:result), finalBoard)
                        Nothing -> algo' k (M.delete (posToKey chosenPos) dom) b

algo = algo' n initialDomain initialBoard



initialDomain:: Domain
initialDomain = mkPosList [(i,j)| i <-[1..n], j<-[1..n]]

initialBoard:: Board
initialBoard = M.fromList assoc
        where assoc = map (\ p -> (posToKey p, 0))  l
              l = [(i,j)| i <-[1..n], j<-[1..n]]

heuristicVal :: Domain -> Position -> Int
heuristicVal dom pos = length $ dom M.\\ (threatenPosition pos )

selectPos :: Domain -> Position
selectPos dom = let listPositions = getPositions dom 
                    f pos1 pos2 = heuristicVal dom pos1 `compare` heuristicVal dom pos2
                in L.maximumBy f listPositions


dumpSolution :: [Position] -> String 
dumpSolution l = showArray board
    where initialArray = A.listArray ((1,1), (n,n)) $ take (n * n ) $ repeat [0]
          board = foldr f initialArray l 
          f el acc = acc A.// [(el, [1])]
          showRow ar i = show [ar A.! (i,j) | j <- [1..n]]
          showArray ar = unlines $ map (showRow ar) [1..n]
