import qualified Data.List as L
type Var = Int
type Position = (Int, Int)
type Domain = [Position]

n = 20
initialDomain:: Domain
initialDomain = [(i,j)| i <-[1..n], j<-[1..n]]
threatenPosition :: Position -> [Position]
threatenPosition (i,j) = [(i,j)] ++ col  ++ row ++ diagSE ++ diagNE ++ diagNW ++ diagSW
    where col = [(k,j) | k <- [1..n], k /= i]
          row = [(i,k) | k <- [1..n], k /= j]
          diagSE = [(i+l, j-l) | l <- [1..min (n-i) (j-1)]]
          diagNE = [(i+l, j+l) | l <- [1..min (n-i) (n-j)]]
          diagNW = [(i-l, j+l) | l <- [1..min (i-1) (n-j)]]
          diagSW = [(i-l, j-l) | l <- [1..min (i-1) (j-1)]]

heuristic :: Domain -> Position
heuristic x =
    where min $ len $ threatenPosition x

algo :: Int -> Domain -> Maybe [Position]
algo _ [] = Nothing 
algo 1 (p:_) = Just [p]  
algo n d@(x:ps) = case algo (n-1) d' of 
                        Just result -> Just (x:result)
                        Nothing -> algo n ps
    where d' = d L.\\ threatenPosition x
