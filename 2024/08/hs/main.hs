import Data.List(sortBy, groupBy, sort, group, tails)

main = do
	task0 "../input/example.txt"
	task0 "../input/input.txt"
	task1 "../input/example.txt"
	task1 "../input/input.txt"

task0 = task ModeNear
task1 = task ModeFar

task mode path = do
	input <- readFile path

	let lineList = lines input
	let w = length (lineList !! 0)
	let h = length lineList

	let anteList = getAnteList input :: [[((Int, Int), Char)]]
	let antePosList = map (map (\(v, _) -> v)) anteList :: [[(Int, Int)]]
	let antinodeListUnfiltered
		= concat $ map (getAntinodeList mode) antePosList :: [(Int, Int)]
	let antinodeList = filter (inMap (w, h)) antinodeListUnfiltered

	print $ length . group . sort $ antinodeList

sortNGroup :: [((Int, Int), Char)] -> [[((Int, Int), Char)]]
sortNGroup xs = groupBy (\(_, c0) (_, c1) -> c0 == c1)
              $ sortBy (\(_, c0) (_, c1) -> compare c0 c1) xs

getAnteList :: String -> [[((Int, Int), Char)]]
getAnteList input =
	let lineList = lines input
	    w = length (lineList !! 0)
	    h = length lineList

	    str = filter (/= '\r') $ filter (/= '\n') input
	    posList = [(x, y) | y <- [0 .. h - 1], x <- [0 .. w - 1]]
	    strNPos = zip posList str :: [((Int, Int), Char)]
	    anteListUngrouped = filter (\(_, c) -> c /= '.') strNPos
	in  sortNGroup anteListUngrouped

data SignalMode = ModeNear | ModeFar
maxSignalDist = 120

getAntinodeList :: SignalMode -> [(Int, Int)] -> [(Int, Int)]
getAntinodeList _ [] = []
getAntinodeList mode (x:xs) = result ++ getAntinodeList mode xs
	where result = concat $ map (\v0 -> getAntinodes mode v0 x) $ tails xs !! 0

getAntinodes :: SignalMode -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
getAntinodes ModeNear (x0, y0) (x1, y1)
	= [(x0 - dx, y0 - dy), (x1 + dx, y1 + dy)]
	where dx = x1 - x0
	      dy = y1 - y0
getAntinodes ModeFar (x0, y0) (x1, y1)
	= signalListBackward ++ signalListForward ++ [(x0, y0), (x1, y1)]
	where dx = x1 - x0
	      dy = y1 - y0
	      signalListBackward
	          = map (\i -> (x0 - dx*i, y0 - dy*i)) [1 .. maxSignalDist]
	      signalListForward
	          = map (\i -> (x1 + dx*i, y1 + dy*i)) [1 .. maxSignalDist]

inMap (mw, mh) (x, y) = x >= 0 && y >= 0 && x < mw && y < mh
