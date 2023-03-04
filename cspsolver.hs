
import qualified Data.List as L
import qualified Data.IntMap.Strict as M
import Data.Maybe
import PosList


type Var = Int
type Domain = PosList


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

algo' :: Int -> Domain -> Maybe [Position]
algo' n dom 
    | M.null dom = Nothing
    | n==1 = Just $ [head $ getPositions dom]
    | otherwise = let chosenPos = selectPos dom
                      dom' = dom M.\\ threatenPosition chosenPos
                  in case algo' (n-1) dom' of
                        Just result -> Just $ chosenPos:result
                        Nothing -> algo' n $ M.delete (posToKey chosenPos) dom

algo = algo' n initialDomain

{- 
algo _ [] = Nothing 
algo 1 (p:_) = Just [p]  
algo n d@(x:ps) = case algo (n-1) d' of 
                        Just result -> Just (x:result)
                        Nothing -> algo n ps
    where d' = d M.\\ threatenPosition x
-}


initialDomain:: Domain
initialDomain = mkPosList [(i,j)| i <-[1..n], j<-[1..n]]

heuristicVal :: Domain -> Position -> Int
heuristicVal dom pos = length $ dom M.\\ (threatenPosition pos )

selectPos :: Domain -> Position
selectPos dom = let listPositions = getPositions dom 
                    f pos1 pos2 = heuristicVal dom pos1 `compare` heuristicVal dom pos2
                in L.maximumBy f listPositions
