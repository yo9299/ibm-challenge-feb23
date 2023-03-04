module PosList where
import qualified Data.IntMap.Strict as M
import Data.Maybe
type PosList = M.IntMap ()
type Position = (Int, Int)
type Domain = PosList

n = 20
existsIn :: Position -> PosList -> Bool
existsIn pos pl = isJust $ M.lookup (posToKey pos) pl 

getPositions :: PosList -> [Position]
getPositions pl = map keyToPos keys
    where keys = M.keys pl 

keyToPos :: Int -> Position
keyToPos k = ( k `div` (n+1), k `mod` (n+1))

posToKey :: Position -> Int
posToKey (i,j) = i*(n+1) +j

mkPosList :: [Position] -> PosList
mkPosList l = M.fromList assoc
    where assoc = map (\ p -> (posToKey p, ())) l

selectHeadPos:: Domain -> Position
selectHeadPos dom 
    | M.null dom = error "selectPos: called with empty domain"
    | otherwise = head $ getPositions dom