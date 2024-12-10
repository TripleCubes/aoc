main = do
	task_0 "../input/example.txt"
	task_0 "../input/input.txt"

task_0 path = do
	input <- readFile path
	let line_list = lines input
	let w = length $ line_list !! 0
	let h = length line_list

	let list = map (\c -> read [c])
	         $ filter (\c -> c /= '\n' && c /= '\r') input :: [Int]
	let list_with_pos
		= zip [(x, y) | y <- [0 .. h - 1], x <- [0 .. w - 1]] list
		:: [((Int, Int), Int)]
	let filtered = filter (\(_, n) -> n == 0) list_with_pos
	let result_list = map (\((x, y), _) -> get_score list w h x y) filtered

	print $ sum result_list

get_score :: [Int] -> Int -> Int -> Int -> Int -> Int
get_score list w h x y = search list w h [(x, y, 0)] 0

search :: [Int] -> Int -> Int -> [(Int, Int, Int)] -> Int -> Int
search list w h queue i
	| i >= length queue = 0
	| otherwise = res + search list w h nx_queue (i + 1)
	where nx_queue = queue ++ up ++ down ++ left ++ right
	      (x, y, n)        = queue !! i
	      (up, n_up)       = dir x (y - 1)
	      (down, n_down)   = dir x (y + 1)
	      (left, n_left)   = dir (x - 1) y
	      (right, n_right) = dir (x + 1) y
	      res = n_up + n_down + n_left + n_right
	      dir x y
	          | not $ in_rect w h x y = ([], 0)
	          | v /= n + 1 = ([], 0)
--	          | elem (x, y, v) queue = ([], 0) -- uncomment this line for part 1
	          | v == 9 = ([(x, y, v)], 1)
	          | otherwise = ([(x, y, v)], 0)
	          where i = index w x y
	                v = list !! i

index w x y = y * w + x
in_rect rw rh x y = x >= 0 && y >= 0 && x < rw && y < rh
