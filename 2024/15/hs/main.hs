import Data.List(elemIndex)
import qualified Data.Array as Arr
import Data.Array((!), (//), Array)
import Data.Ix(range)

type Vec2i = (Int, Int)
data Obs = ObsNone | ObsBox | ObsWall
           deriving (Show, Eq)
type MMap = Array Int Obs

main = do
	task0 "../input/example.txt"
	task0 "../input/input.txt"

task0 path = do
	input <- readFile path
	let (w, h) = getWH input :: Vec2i
	let (inMap, inMoves, robotPos) = parse input :: (MMap, [Vec2i], Vec2i)
	let (finalMap, finalRobotPos) = iter inMap (w, h) robotPos inMoves
	let result = getResult finalMap (w, h)

	print result

getWH :: String -> Vec2i
getWH input = (w, h)
	where input1 = filter (\c -> c /= '\r') input
	      Just separator
	          = foldr1 min [elemIndex '<' input1, elemIndex '>' input1,
	                        elemIndex '^' input1, elemIndex 'v' input1]
	      _map = take (separator - 1) input1

	      lineList = filter (\s -> s /= "") $ lines _map :: [String]
	      w = (length $ lineList !! 0) - 2
	      h = length lineList - 2

parse :: String -> (MMap, [Vec2i], Vec2i)
parse input = (inMap, inMoves, robotPos)
	where input1 = filter (\c -> c /= '\r') input
	      Just separator
	          = foldr1 min [elemIndex '<' input1, elemIndex '>' input1,
	                        elemIndex '^' input1, elemIndex 'v' input1]
	      _map = take (separator - 1) input1
	      moves = drop (separator - 1) input1

	      lineList = filter (\s -> s /= "") $ lines _map :: [String]
	      w = (length $ lineList !! 0) - 2
	      h = length lineList - 2
	      posList
	          = filter (\((x, y), _) -> x /= -1 && y /= -1 && x /= w && y /= h)
	          $ zip (range ((-1, -1), (w, h)))
	          $ filter (\c -> c /= '\n') _map :: [(Vec2i, Char)]
	      robotPos = fst $ filter (\(_, c) -> c == '@') posList !! 0 :: Vec2i

	      inMap = Arr.listArray (0, w * h - 1)
	            $ map (\(_, c) -> case c of 'O' -> ObsBox
	                                        '#' -> ObsWall
	                                        _   -> ObsNone) posList :: MMap
	      inMoves = map (\c -> case c of '<' -> (-1,  0)
	                                     '>' -> ( 1,  0)
	                                     '^' -> ( 0, -1)
	                                     'v' -> ( 0,  1))
	                $ filter (\c -> c /= '\n') moves :: [Vec2i]

getResult :: MMap -> Vec2i -> Int
getResult _map (w, h) = result
	where list = Arr.assocs _map
	      result = sum $ map (\(x, y) -> (x + 1) + 100 * (y + 1))
	                   $ map (posFromIndex . fst)
	                   $ filter (\(i, obs) -> obs == ObsBox) list
	      posFromIndex n = (n `mod` w, n `div` w)

iter :: MMap -> Vec2i -> Vec2i -> [Vec2i] -> (MMap, Vec2i)
iter _map _      (x, y) []           = (_map, (x, y))
iter _map (w, h) (x, y) (move:moves) = iter nxmap (w, h) (nxx, nxy) moves
	where (nxmap, (nxx, nxy)) = step _map (w, h) (x, y) move

step :: MMap -> Vec2i -> Vec2i -> Vec2i -> (MMap, Vec2i)
step _map (w, h) (x, y) (dirx, diry)
	| not $ inRect w h nxx nxy = (_map, (x, y))
	| otherwise = (nxmap, (nxBotX, nxBotY))
	where (nxx, nxy) = (x + dirx, y + diry)
	      nxobs = _map ! (nxy * w + nxx)
	      (nxmap, (nxBotX, nxBotY))
	          = case nxobs of ObsNone -> (_map, (nxx, nxy))
	                          ObsWall -> (_map, (x, y))
	                          ObsBox -> a
	      a :: (MMap, Vec2i)
	      a = case f (nxx, nxy) of
	              Nothing       -> (_map, (x, y))
	              Just (rx, ry) -> (_map // [(ry * w + rx, ObsBox),
	                                         (nxy * w + nxx, ObsNone)],
	                                             (nxx, nxy))

	      f :: Vec2i -> Maybe Vec2i
	      f (x, y)
	          | not $ inRect w h x y = Nothing
	          | obs == ObsWall = Nothing
	          | obs == ObsNone = Just (x, y)
	          | otherwise = f (x + dirx, y + diry)
	          where obs = _map ! (y * w + x)

inRect rw rh x y = x >= 0 && y >= 0 && x < rw && y < rh
