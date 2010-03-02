import System.IO
import Data.List
import Debug.Trace

setBuffers = do
    hSetBuffering stdin LineBuffering
    hSetBuffering stdout LineBuffering

-- main = playBot testBot startingValue
main = playBot openSpaceBot startingValue


playBot :: ([[Spot]] -> a -> (Move, a)) -> a -> IO ()
playBot bot starting = do
    setBuffers
    interact ((playTurns bot starting) . lines)

readInt :: String -> Int
readInt a = read a

readSpot '#' = Wall
readSpot ' ' = Blank
readSpot '1' = Player
readSpot '2' = Enemy

makeMove North = "1"
makeMove East = "2"
makeMove South = "3"
makeMove West = "4"

playTurns bot pastValue [] = ""
playTurns bot pastValue str = (makeMove move) ++ "\n" ++ playTurns bot history (drop (h+1) str)
    where [w,h] = map readInt (words $ head str)
          tronMap = map (map readSpot) (take h (tail str))
	  (move, history) = bot tronMap pastValue

data Spot = Wall | Blank | Player | Enemy | Token deriving Eq
data Move = North | East | South | West deriving (Eq, Enum, Ord)

startingValue = ()

--testBot :: [[Spot]] -> a -> (Move, a)
findGuy player tronMap = (maybe 0 id (findIndex (== player) (head $ filter (any (== player)) tronMap)), maybe 0 id (findIndex (any (== player)) tronMap))

findPlayer = findGuy Player
findEnemy  = findGuy Enemy


me tronMap = (maybe 0 id (findIndex (== Player) (head $ filter (any (== Player)) tronMap)), maybe 0 id (findIndex (any (== Player)) tronMap))

-- list replace with value
lreplace index l v = l'
    where
      (h,t) = splitAt index l
      t' = case t of
             [] -> []
             x:xs -> xs
      l' = h ++ [v] ++ t'

-- list replace with function call on value
lfreplace :: Int -> [a] -> ( a -> a ) -> [a]
lfreplace index l f = l'
    where
      (h,t) = splitAt index l
      (v:t') = t
      l' = h ++ ((f v): t')


tmapGet tronMap x y = (tronMap !! y) !! x
tmapReplace tronMap x y v = lfreplace y tronMap (\l -> lreplace x l v)

canMove move (x,y) tronMap
    | move == North	= if y == 0 then False else (Blank == ((tronMap !! (y-1)) !! x))
    | move == East	= if x+1 == (length (head tronMap)) then False else (Blank == ((tronMap !! y) !! (x+1)))
    | move == South	= if y+1 == (length tronMap) then False else (Blank == ((tronMap !! (y+1)) !! x))
    | move == West	= if x == 0 then False else (Blank == ((tronMap !! y) !! (x-1)))

testBot tronMap b = (head possibleMoves, b)
    where possibleMoves = (filter (\a -> canMove a (me tronMap) tronMap) [North, East, South, West]) ++ [North]


vecOfDir North = (0,-1)
vecOfDir South = (0,1)
vecOfDir East = (1,0)
vecOfDir West = (-1,0)
dirOfVec (0,-1) = North
dirOfVec (0,1) = South
dirOfVec (1,0) = East
dirOfVec (-1,0) = West

dAdd (x,y) (u,v) = (x+u,v+y)
dSub (x,y) (u,v) = (x - u, y - v)

directionsOf (x,y) = [(x-1,y),(x+1,y),(x,y+1),(x,y-1)]
directionsOfwDirs v = map (\d -> (d,dAdd v (vecOfDir d)))  [North,East,South,West]
-- Tries to count the open spaces
-- assumption: there are walls
openSpaceFrom acc tronMap (x,y) =
    if (acc > 10) 
    then acc
    else
        let here = tmapGet tronMap x y  in
        case here of
          -- this will make a move no matter what at least
          Blank -> let tmap = tmapReplace tronMap x y Token  in
                   maximum (map (openSpaceFrom (acc+1) tmap) (directionsOf (x,y)))
          _ -> acc -- anything else is blocked so don't respond


-- openSpaceBot tries to find the most openspace to move into
openSpaceBot tronMap b = ( finalDirection  , b)
    where
      (px,py) = findPlayer tronMap
      (ox,oy) = findEnemy  tronMap
      pdirs = directionsOfwDirs (px,py)
      (v,d) = maximum (map (\(d,(x,y)) -> ((openSpaceFrom 0 tronMap (x,y)),d)) pdirs)
      finalDirection = d
      


sqr :: Int -> Int 
sqr x = x * x
                
runTest = do
    print ( show ( lfreplace 4 [1..10] sqr ) )
