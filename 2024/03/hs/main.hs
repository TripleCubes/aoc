main = do
	task_0 "../input/example.txt"
	task_0 "../input/input.txt"
	task_1 "../input/example1.txt"
	task_1 "../input/input.txt"

task_0 path = do
	str <- readFile path
	let mul_list = find_str str "mul" 0
	let bracked_list = find_bracked str 0 0 "" "" Zero
	let pair_list = [(a, b) | x <- mul_list, (y, a, b) <- bracked_list, y - x == 3]
	let result = sum $ map (\(a, b) -> a * b) pair_list
	print result

task_1 path = do
	str <- readFile path
	let mul_list = find_str str "mul" 0
	let bracked_list = find_bracked str 0 0 "" "" Zero
	let trip_list = [(x, a, b) | x <- mul_list, (y, a, b) <- bracked_list, y - x == 3]
	let do_don't_list = find_do_don't str
	let calc (x, a, b) = if do_don't == Don't then 0 else a * b
		where do_don't = get_do_don't_at x do_don't_list
	let result = sum $ map calc trip_list
	print result

find_str :: String -> String -> Int -> [Int]
find_str xs fs _
	| length xs < length fs = []
find_str xs fs i = if has_str xs fs then i:find_str (drop flen xs) fs (i + flen)
                                    else find_str (drop 1 xs) fs (i + 1)
	where flen = length fs

has_str xs fs = take (length fs) xs == fs


data Found = Zero | OpenBracket | FirstNum | Comma | SecNum

find_bracked :: String -> Int -> Int -> String -> String -> Found -> [(Int, Int, Int)]
find_bracked [] _ _ _ _ _ = []
find_bracked (x:xs) i i0 _ _ Zero
	| x == '(' = find_bracked xs (i + 1) i "" "" OpenBracket
find_bracked (x:xs) i i0 s0 s1 OpenBracket
	| is_num x = find_bracked xs (i + 1) i0 (s0 ++ [x]) s1 FirstNum
find_bracked (x:xs) i i0 s0 s1 FirstNum
	| is_num x = find_bracked xs (i + 1) i0 (s0 ++ [x]) s1 FirstNum
	| x == ',' = find_bracked xs (i + 1) i0 s0 s1 Comma
find_bracked (x:xs) i i0 s0 s1 Comma
	| is_num x = find_bracked xs (i + 1) i0 s0 (s1 ++ [x]) SecNum
find_bracked (x:xs) i i0 s0 s1 SecNum
	| is_num x = find_bracked xs (i + 1) i0 s0 (s1 ++ [x]) SecNum
	| x == ')' = (i0, read s0, read s1):find_bracked xs (i + 1) i0 s0 s1 Zero
find_bracked (x:xs) i _ _ _ _ = find_bracked xs (i + 1) 0 "" "" Zero

is_num x = x >= '0' && x <= '9'


data DoDon't = Do | Don't deriving (Show, Eq)

find_do_don't :: String -> [(Int, DoDon't)]
find_do_don't str = combine_do_don't do_list don't_list
	where do_list = find_str str "do()" 0
	      don't_list = find_str str "don't()" 0

combine_do_don't :: [Int] -> [Int] -> [(Int, DoDon't)]
combine_do_don't [] xs = map (\x -> (x, Don't)) xs
combine_do_don't xs [] = map (\x -> (x, Do)) xs
combine_do_don't (x:do_xs) (y:don't_ys)
	| x < y = (x, Do):combine_do_don't do_xs (y:don't_ys)
	| x > y = (y, Don't):combine_do_don't (x:do_xs) don't_ys


get_do_don't_at :: Int -> [(Int, DoDon't)] -> DoDon't
get_do_don't_at _ [] = Do
get_do_don't_at i ((ix, d):(nx, nd):xs)
	| i > ix && i < nx = d
	| otherwise = get_do_don't_at i ((nx, nd):xs)
get_do_don't_at i ((ix, d):xs)
	| i > ix = d
	| otherwise = Do
