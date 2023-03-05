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

numberOfIndependentSafeSpaces :: Domain -> Int
numberOfIndependentSafeSpaces spaces = length indepSpaces
    where indepSpaces = [sp | sp <- getPositions spaces,
                              [thp | thp <- kThreatenPos' sp, isJust $ posToKey thp `M.lookup` spaces] == [sp]]
    
kThreatenPos:: Position -> PosList
kThreatenPos = mkPosList . kThreatenPos' 
kThreatenPos' :: Position -> [Position]
kThreatenPos' (i,j) = filter (\ (k,l) -> k >= 1 && l >= 1 && k <= n && l <= n) candidates
    where candidates = [(i-1,j-1), (i-1, j), (i-1, j+1), 
                        (i, j-1), (i,j), (i, j+1),
                        (i+1, j-1), (i+1, j), (i+1, j+1)]

kAlgo :: Int -> Domain -> PartialSolution -> Maybe [Solution]
kAlgo k dom partSol
    | M.null dom = Just []
    | M.size dom < k = Just []
    | k==1 = let totalsols = fmap L.sort $ map (: partSol) (getPositions dom)
             in trace ("[leaf] found " ++ show (length totalsols) ++ " king assignments") $ 
                    if length totalsols <= 48 
                        then Just totalsols
                        else Nothing
    | otherwise = let explore :: Position -> Domain -> Maybe [Solution]
                      explore pos d = let dom' = d M.\\ (kThreatenPos pos)
                                          partSol' = pos:partSol
                                      in kAlgo (k-1) dom' partSol'
                      f el Nothing = Nothing
                      f el (Just (sols, d))  = let ret = explore el d
                                                   newsols = L.nub $ fromJust ret ++ sols 
                                               in if  isNothing ret then trace ("[depth=" ++ show k ++ "] cutting") Nothing
                                                  else if length newsols <= 48
                                                       then trace ("[depth=" ++ show k ++ "] current size= " ++ show (length newsols)) $
                                                                Just (newsols, M.delete (posToKey el) d) 
                                                       else trace ("[depth="  ++ show k ++ "] stopping king solver: " ++ show (length newsols) ++ " solutions have been enumerated.") 
                                                                Nothing 
                  in fmap fst (foldr f (Just ([], dom)) $ getPositions dom)
