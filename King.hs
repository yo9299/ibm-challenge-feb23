module King where
import PosList
import qualified Data.IntMap.Strict as M

type Board = M.IntMap Int
type Solution = [Position]
type PartialSolution = [Position]

safeSpaces :: Board -> Domain
safeSpaces b = fmap (\_ -> ()) $ M.filter (==2) b 

kThreatenPos :: Position -> PosList
kThreatenPos (i,j) = mkPosList $ filter (\ (k,l) -> k >= 1 && l >= 1 && k <= n && l <= n) candidates
    where candidates = [(i-1,j-1), (i-1, j), (i-1, j+1), 
                        (i, j-1), (i,j), (i, j+1),
                        (i+1, j-1), (i+1, j), (i+1, j+1)]

kAlgo :: Int -> Domain -> PartialSolution -> [Solution]
kAlgo k dom partSol
    | M.null dom = []
    | k==1 = map (: partSol) (getPositions dom)
    | otherwise = let explore :: Position -> [Solution]
                      explore pos = let dom' = dom M.\\ (kThreatenPos pos)
                                        partSol' = pos:partSol
                                    in kAlgo (k-1) dom' partSol'
                  in concatMap explore (getPositions dom)
