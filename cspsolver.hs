
import qualified Data.List as L
import qualified Data.Array as A
import qualified Data.IntMap.Strict as M
import Data.Maybe
import PosList


type Var = Int
type Domain = PosList
type Board = M.IntMap Int

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



selectHeadPos:: Domain -> Position
selectHeadPos dom 
    | M.null dom = error "selectPos: called with empty domain"
    | otherwise = head $ getPositions dom

algo' :: Int -> Domain -> Board -> Maybe ([Position] , Board)
algo' n dom b
    | M.null dom = Nothing
    | n==1 = let newPos = head $ getPositions dom
             in Just  ([newPos], updateBoard newPos b) 
    | otherwise = let chosenPos = selectPos dom
                      dom' = dom M.\\ threatenPosition chosenPos
                      b' = updateBoard chosenPos b
                  in case algo' (n-1) dom' b' of
                        Just (result, finalBoard) -> Just ((chosenPos:result), finalBoard)
                        Nothing -> algo' n (M.delete (posToKey chosenPos) dom) b

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
