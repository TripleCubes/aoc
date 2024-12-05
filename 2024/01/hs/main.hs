import Data.List(sort)

main = do
	get_result_0 "../input/example.txt"
	get_result_0 "../input/input.txt"
	get_result_1 "../input/example.txt"
	get_result_1 "../input/input.txt"

get_result_0 path = do
	str <- readFile path
	let (l_list, r_list) = to_lr $ map read $ words str
	let result = sum $ map (\(l, r) -> abs (l - r)) $ zip (sort l_list) (sort r_list)
	print result

get_result_1 path = do
	str <- readFile path
	let (l_list, r_list) = to_lr $ map read $ words str
	let result = sum $ map (\x -> x * count x r_list) l_list
	print result

count n = length . filter (== n)

to_lr :: [Int] -> ([Int], [Int])
to_lr [] = ([], [])
to_lr (l:r:xs) = (l:l_list, r:r_list)
	where (l_list, r_list) = to_lr xs
