main = do
	task_0 "../input/example.txt"
	task_0 "../input/input.txt"
	task_1 "../input/example.txt"
	task_1 "../input/input.txt"

task_0 path = do
	task path (op_list 2) 2

task_1 path = do
	task path (op_list 3) 3

task path op_list num_ops = do
	input <- readFile path
	let list_str = map (filter (/= ':')) $ lines input :: [String]
	let list_str_1 = map words list_str :: [[String]]
	let list_num = map (map read) list_str_1 :: [[Int]]
	let list_pair = map (\(x:xs) -> (x, xs)) list_num :: [(Int, [Int])]
	let result_list
		= map (get_results op_list num_ops) list_pair :: [[(Int, Bool)]]
	let result_list_filtered
		= map (filter (\(_, b) -> b == True)) result_list :: [[(Int, Bool)]]
	let result_list_mapped
		= map (map (\(n, _) -> n)) result_list_filtered :: [[Int]]
	print $ sum
	      $ map (\x -> if length x == 0 then 0 else x !! 0) result_list_mapped


op_list_sz = 15

get_results :: [[Int -> Int -> Int]] -> Int -> (Int, [Int]) -> [(Int, Bool)]
get_results op_list num_ops (test_v, num_list)
	= map (get_calc_comp test_v num_list) _ops_list
	where ops_len = length num_list - 1
	      _ops_list = take (num_ops ^ ops_len)
	                $ map (take ops_len) op_list
	                                   :: [[Int -> Int -> Int]]

get_calc_comp :: Int -> [Int] -> [Int -> Int -> Int] -> (Int, Bool)
get_calc_comp test num_list ops = (result, result == test)
	where result = get_calc (reverse num_list) ops

get_calc :: [Int] -> [Int -> Int -> Int] -> Int
get_calc [n] _ = n
get_calc (n:num_list) (x:ops) = get_calc num_list ops `x` n

op_list :: Int -> [[Int -> Int -> Int]]
op_list = op_list_n op_list_sz

op_list_n :: Int -> Int -> [[Int -> Int -> Int]]
op_list_n max num_ops = map f [0 .. num_ops ^ max - 1]
	where f n = op_list ++ (take (max - ops_len) (repeat (+)))
	          where op_list = to_op_list n num_ops
	                ops_len = length op_list

to_op_list :: Int -> Int -> [Int -> Int -> Int]
to_op_list 0 _ = []
to_op_list n num_ops = op : to_op_list (n `div` num_ops) num_ops
	where m = n `mod` num_ops
	      op = if      m == 0 then (+)
	           else if m == 1 then (*)
	           else                merge_num

merge_num a b = 10 ^ b_len * a + b
	where b_len = get_num_digits b

get_num_digits 0 = 0
get_num_digits n = 1 + get_num_digits (n `div` 10)
