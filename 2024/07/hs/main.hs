main = do
	task_0 "../input/example.txt"
	task_0 "../input/input.txt"
	task_1 "../input/example.txt"
	task_1 "../input/input.txt"

task_0 path = do
	input <- readFile path
	let list_str = map (filter (/= ':')) $ lines input :: [String]
	let list_str_1 = map words list_str :: [[String]]
	let list_num = map (map read) list_str_1 :: [[Int]]
	let list_pair = map (\(x:xs) -> (x, xs)) list_num :: [(Int, [Int])]
	let result_list = map (get_results op_list_0 2) list_pair :: [[(Int, Bool)]]
	let result_list_filtered
		= map (filter (\(_, b) -> b == True)) result_list :: [[(Int, Bool)]]
	let result_list_mapped
		= map (map (\(n, _) -> n)) result_list_filtered :: [[Int]]
	print $ sum
	      $ map (\x -> if length x == 0 then 0 else x !! 0) result_list_mapped

task_1 path = do
	input <- readFile path
	let list_str = map (filter (/= ':')) $ lines input :: [String]
	let list_str_1 = map words list_str :: [[String]]
	let list_num = map (map read) list_str_1 :: [[Int]]
	let list_pair = map (\(x:xs) -> (x, xs)) list_num :: [(Int, [Int])]
	let result_list = map (get_results op_list_1 3) list_pair :: [[(Int, Bool)]]
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
	                $ map (drop (op_list_sz - ops_len)) op_list
	                                   :: [[Int -> Int -> Int]]

get_calc_comp :: Int -> [Int] -> [Int -> Int -> Int] -> (Int, Bool)
get_calc_comp test num_list ops = (result, result == test)
	where result = get_calc (reverse num_list) (reverse ops)

get_calc :: [Int] -> [Int -> Int -> Int] -> Int
get_calc [n] _ = n
get_calc (n:num_list) (x:ops) = get_calc num_list ops `x` n

op_list_0 :: [[Int -> Int -> Int]]
op_list_0 = op_list_0_n op_list_sz

op_list_0_n :: Int -> [[Int -> Int -> Int]]
op_list_0_n max = map f [0 .. 2 ^ max - 1]
	where f n = (take (max - ops_len) (repeat (+))) ++ op_list
	          where op_list = to_op_list_0 n
	                ops_len = length op_list

to_op_list_0 :: Int -> [Int -> Int -> Int]
to_op_list_0 0 = []
to_op_list_0 n = to_op_list_0 (n `div` 2) ++ [op]
	where op = if n `mod` 2 == 0 then (+) else (*)


op_list_1 :: [[Int -> Int -> Int]]
op_list_1 = op_list_1_n op_list_sz

op_list_1_n :: Int -> [[Int -> Int -> Int]]
op_list_1_n max = map f [0 .. 3 ^ max - 1]
	where f n = (take (max - ops_len) (repeat (+))) ++ op_list
	          where op_list = to_op_list_1 n
	                ops_len = length op_list

to_op_list_1 :: Int -> [Int -> Int -> Int]
to_op_list_1 0 = []
to_op_list_1 n = to_op_list_1 (n `div` 3) ++ [op]
	where m = n `mod` 3
	      op = if      m == 0 then (+)
	           else if m == 1 then (*)
	           else                merge_num

merge_num a b = 10 ^ b_len * a + b
	where b_len = get_num_digits b

get_num_digits 0 = 0
get_num_digits n = 1 + get_num_digits (n `div` 10)
