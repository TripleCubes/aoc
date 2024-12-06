import Data.List(elemIndex, group, sort)

main = do
	task_0 "../input/example.txt"
	task_0 "../input/input.txt"
	task_1 "../input/example.txt"
	task_1 "../input/input.txt"

task_0 input_path = do
	input <- readFile input_path
	let w         :: Int          = length $ lines input !! 0
	    h         :: Int          = length $ lines input
	    cmap      :: [Char]       = filter (\c -> c /= '\n' && c /= '\r') input
	    pos_map   :: [((Int, Int), Char)]
	               = zip [(x, y) | y <- [0 .. h - 1], x <- [0 .. w - 1]] cmap
	    guard_pos :: (Int, Int)
	               = fst $ filter (\(_, c) -> c == '^') pos_map !! 0
	    path      :: [((Int, Int), Bool)]
	               = get_path cmap (w, h) DirUp 0 guard_pos [] (-1, -1)
	print $ length . group . sort $ map (fst) path

task_1 input_path = do
	input <- readFile input_path
	let w         :: Int          = length $ lines input !! 0
	    h         :: Int          = length $ lines input
	    cmap      :: [Char]       = filter (\c -> c /= '\n' && c /= '\r') input
	    pos_map   :: [((Int, Int), Char)]
	               = zip [(x, y) | y <- [0 .. h - 1], x <- [0 .. w - 1]] cmap
	    guard_pos :: (Int, Int)
	               = fst $ filter (\(_, c) -> c == '^') pos_map !! 0
	    path      :: [((Int, Int), Bool)]
	               = get_path cmap (w, h) DirUp 0 guard_pos [] (-1, -1)
	    pos_list :: [(Int, Int)]
	               = map (\(x:xs) -> x) $ group $ sort $ map (fst) path

	    path_list :: [[((Int, Int), Bool)]]
	               = map (get_path cmap (w, h) DirUp 0 guard_pos []) pos_list
	    turn_point_list_list :: [[((Int, Int), Bool)]]
	               = map (filter (\(_, is_turn) -> is_turn == True)) path_list

	    have_dup_list :: [Bool] = map (have_dup) turn_point_list_list

	print $ length . filter (== True) $ have_dup_list


data Dir = DirUp | DirRight | DirDown | DirLeft deriving (Enum, Ord, Eq)

get_path :: [Char] -> (Int, Int) -> Dir -> Int -> (Int, Int) -> [(Int, Int)] -> (Int, Int) -> [((Int, Int), Bool)]
get_path cmap map_sz dir step pos turn_points obs_pos
	| not $ in_map_tup $ next_pos dir x y = [result_tup]
	| is_turn && elem (x, y) turn_points = [result_tup]
	| otherwise = result_tup:nx_path
	where (x, y)         = pos
	      (w, h)         = map_sz
	      result_tup     = ((x, y), is_turn)
	      ((nx_x, nx_y), nx_dir, is_turn) = next_dest_dir dir x y False
	      nx_turn_points = if is_turn then (x, y):turn_points else turn_points
	      nx_path = get_path cmap map_sz nx_dir (step + 1) (nx_x, nx_y) nx_turn_points obs_pos

	      index x y      = y * w + x
	      in_map x y     = x >= 0 && y >= 0 && x < w && y < h
	      in_map_tup (x, y) = in_map x y

	      next_dir DirLeft = DirUp
	      next_dir dir  = succ dir

	      next_pos DirUp x y    = (x, y - 1)
	      next_pos DirDown x y  = (x, y + 1)
	      next_pos DirLeft x y  = (x - 1, y)
	      next_pos DirRight x y = (x + 1, y)

	      next_dest_dir :: Dir -> Int -> Int -> Bool -> ((Int, Int), Dir, Bool)
	      next_dest_dir dir x y is_turn
	          | (nx_x, nx_y) == obs_pos = next_dest_dir (next_dir dir) x y True
	          | nx_c == '#' = next_dest_dir (next_dir dir) x y True
	          | otherwise   = ((nx_x, nx_y), dir, is_turn)
	          where nx_c         = if in_map nx_x nx_y then cmap !! index nx_x nx_y
	                                                   else '.'
	                (nx_x, nx_y) = next_pos dir x y

have_dup :: Eq a => [a] -> Bool
have_dup [] = False
have_dup [x] = False
have_dup (x:xs)
	| elem x xs = True
	| otherwise = have_dup xs
