module King where
 
import PosList
import Debug.Trace
import Data.Maybe
import qualified Data.IntMap.Strict as M
import qualified Data.List as L

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

kAlgo :: Int -> Domain -> PartialSolution -> Maybe [Solution]
kAlgo k dom partSol
    | M.null dom = Just []
    | k==1 = let totalsols = fmap L.sort $ map (: partSol) (getPositions dom)
             in trace ("found " ++ show (length totalsols) ++ " king assignments") $ Just totalsols
    | otherwise = let explore :: Position -> Maybe [Solution]
                      explore pos = let dom' = dom M.\\ (kThreatenPos pos)
                                        partSol' = pos:partSol
                                    in kAlgo (k-1) dom' partSol'
                      f el Nothing = Nothing
                      f el (Just sols) = let ret = explore el
                                             newsols = L.nub $ fromJust ret ++ sols 
                                         in if  isNothing ret then Nothing
                                            else if length newsols <= 48
                                                 then Just newsols 
                                                 else trace ("stopping king solver: " ++ show (length newsols) ++ " solutions have been enumerated.") Nothing 
                  in foldr f (Just []) $ getPositions dom
