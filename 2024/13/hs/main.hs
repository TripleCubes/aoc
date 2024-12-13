import Debug.Trace(trace)

debug = flip trace

type Vec2i = (Int, Int)

main = do
	task0 "../input/example.txt"
	task0 "../input/input.txt"
	task1 "../input/example.txt"
	task1 "../input/input.txt"

task0 path = do
	input <- readFile path
	let inputFiltered
		= filter (\c -> (c >= '0' && c <= '9') || c == '\n' || c == ' ') input
	let wordList = map words $ lines inputFiltered :: [[String]]
	let numList = filter (\x -> x /= []) $ map (map read) wordList :: [[Int]]
	let vec2iList = map (\(x:y:_) -> (x, y)) numList
	let pack3List = toPack3 vec2iList
	print $ sum $ map getResult pack3List

task1 path = do
	input <- readFile path
	let inputFiltered
		= filter (\c -> (c >= '0' && c <= '9') || c == '\n' || c == ' ') input
	let wordList = map words $ lines inputFiltered :: [[String]]
	let numList = filter (\x -> x /= []) $ map (map read) wordList :: [[Int]]
	let vec2iList = map (\(x:y:_) -> (x, y)) numList
	let pack3List = toPack3 vec2iList
	let add = 10000000000000
	let pack3ListInserted
		= map (\(a, b, (x, y)) -> (a, b, (x + add, y + add))) pack3List
	print $ sum $ map getResult pack3ListInserted

getResult :: (Vec2i, Vec2i, Vec2i) -> Int
getResult ((ax, ay), (bx, by), (destx, desty)) = cost
	where (a, b) = solve (ax, ay) (bx, by) (destx, desty)
	      (destx', desty') = (a * ax + b * bx, a * ay + b * by)
	      cost = if destx' == destx && desty' == desty then a * 3 + b
	                                                   else 0

toPack3 :: [Vec2i] -> [(Vec2i, Vec2i, Vec2i)]
toPack3 [] = []
toPack3 (x:y:z:list) = (x, y, z) : toPack3 list

commonDown a b
	| a > b = commonDown (a - b) b
	| b > a = commonDown a (b - a)
	| otherwise = a

commonUp a b = (a * b) `div` (commonDown a b)

solve :: Vec2i -> Vec2i -> Vec2i -> Vec2i
solve (ax, ay) (bx, by) (cx, cy) = (a, b)
	where aCommonUp = commonUp ax ay
	      xMul = aCommonUp `div` ax
	      yMul = aCommonUp `div` ay
	      bXY = bx * xMul - by * yMul
	      c = xMul * cx - yMul * cy
	      b = c `div` bXY
	      a = (cx - bx * b) `div` ax