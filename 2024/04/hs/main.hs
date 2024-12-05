main = do
	task_0 "../input/example.txt"
	task_0 "../input/input.txt"
	putStrLn "============================="
	task_1 "../input/example.txt"
	task_1 "../input/input.txt"

task_0 path = do
	str <- readFile path
	let str_find = "XMAS"
	let str_list = lines str
	let result_up           = length $ search str_list str_find Vert Backward
	let result_down         = length $ search str_list str_find Vert Forward
	let result_left         = length $ search str_list str_find Hori Backward
	let result_right        = length $ search str_list str_find Hori Forward
	let result_top_left     = length $ search str_list str_find DiagDown Backward
	let result_top_right    = length $ search str_list str_find DiagUp Forward
	let result_bottom_left  = length $ search str_list str_find DiagUp Backward
	let result_bottom_right = length $ search str_list str_find DiagDown Forward
	putStrLn $ "up           " ++ show result_up
	putStrLn $ "down         " ++ show result_down
	putStrLn $ "left         " ++ show result_left
	putStrLn $ "right        " ++ show result_right
	putStrLn $ "top_left     " ++ show result_top_left
	putStrLn $ "top_right    " ++ show result_top_right
	putStrLn $ "bottom_left  " ++ show result_bottom_left
	putStrLn $ "bottom_right " ++ show result_bottom_right
	print $ result_up + result_down + result_left + result_right
	                  + result_top_left + result_top_right
	                  + result_bottom_left + result_bottom_right

task_1 path = do
	str <- readFile path
	let str_find = "MAS"
	let str_list = lines str
	let diag_up_list   = search str_list str_find DiagUp Forward
	                  ++ search str_list str_find DiagUp Backward
	let diag_down_list = search str_list str_find DiagDown Forward
	                  ++ search str_list str_find DiagDown Backward
	let diag_up_pos_list   = map (\(x, y, _) -> (x + 1, y - 1)) diag_up_list
	let diag_down_pos_list = map (\(x, y, _) -> (x + 1, y + 1)) diag_down_list
	let pos_list = [x | x <- diag_up_pos_list, y <- diag_down_pos_list, x == y]
	print $ length pos_list

data Plane = Hori | Vert | DiagUp | DiagDown
data Dir = Forward | Backward deriving (Eq)

search :: [String] -> String -> Plane -> Dir -> [(Int, Int, String)]
search str_xs fs Hori dir =
	let pos_list = [(x, y) | y <- [0 .. length str_xs - 1],
	                         x <- [0 .. length (str_xs!!0) - length fs + 1 - 1]]
	in  filter (\(x, y, str) -> str == fs) .
	    map (rev_str_of_triple dir) .
	    map (\(x, y) -> (x, y, get_chars_hori (str_xs !! y) (length fs) x)) $ pos_list
search str_xs fs Vert dir =
	let pos_list = [(x, y) | y <- [0 .. length str_xs - length fs + 1 - 1],
	                         x <- [0 .. length (str_xs!!0) - 1]]
	in  filter (\(x, y, str) -> str == fs) .
	    map (rev_str_of_triple dir) .
	    map (\(x, y) -> (x, y, get_chars_vert str_xs (length fs) x y)) $ pos_list
search str_xs fs DiagUp dir =
	let w = length $ str_xs !! 0
	    h = length str_xs
	    pos_list = [(x, y) | x <- [0 .. w - 1], y <- [0 .. h - 1],
	                         in_rect (x + length fs - 1) (y - length fs + 1) w h]
	in  filter (\(x, y, str) -> str == fs) .
	    map (rev_str_of_triple dir) .
	    map (\(x, y) -> (x, y, get_chars_diag_up str_xs (length fs) x y)) $ pos_list
search str_xs fs DiagDown dir =
	let w = length $ str_xs !! 0
	    h = length str_xs
	    pos_list = [(x, y) | x <- [0 .. w - 1], y <- [0 .. h - 1],
	                         in_rect (x + length fs - 1) (y + length fs - 1) w h]
	in  filter (\(x, y, str) -> str == fs) .
	    map (rev_str_of_triple dir) .
	    map (\(x, y) -> (x, y, get_chars_diag_down str_xs (length fs) x y)) $ pos_list

rev_str_of_triple dir (x, y, str) = if dir == Forward then (x, y, str) else (x, y, reverse str)
in_rect x y rw rh = x >= 0 && y >= 0 && x < rw && y < rh

get_chars_hori :: String -> Int -> Int -> String
get_chars_hori xs len i = take len . drop i $ xs

get_chars_vert :: [String] -> Int -> Int -> Int -> String
get_chars_vert _ 0 _ _ = ""
get_chars_vert str_xs len x y = str_xs !! y !! x :
                                get_chars_vert str_xs (len - 1) x (y + 1)

get_chars_diag_up :: [String] -> Int -> Int -> Int -> String
get_chars_diag_up _ 0 _ _ = ""
get_chars_diag_up str_xs len x y = str_xs !! y !! x :
                                   get_chars_diag_up str_xs (len - 1) (x + 1) (y - 1)

get_chars_diag_down :: [String] -> Int -> Int -> Int -> String
get_chars_diag_down _ 0 _ _ = ""
get_chars_diag_down str_xs len x y = str_xs !! y !! x :
                                     get_chars_diag_down str_xs (len - 1) (x + 1) (y + 1)
