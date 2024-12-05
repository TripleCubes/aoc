main = do
	task_0 "../input/example.txt"
	task_0 "../input/input.txt"
	task_1 "../input/example.txt"
	task_1 "../input/input.txt"

task_0 path = do
	str <- readFile path
	let line_list = map (map read) $ map words $ lines str
	let result = length . filter (== True) $ map check line_list
	print result

task_1 path = do
	str <- readFile path
	let line_list = map (map read) $ map words $ lines str
	let result = length . filter (== True) $ map check_all_skip_cases line_list
	print result


check :: [Int] -> Bool
check (x:y:xs) = if x > y then check_sign (>) (x:y:xs)
                          else check_sign (<) (x:y:xs)

check_sign :: (Int -> Int -> Bool) -> [Int] -> Bool
check_sign p (x:y:xs)
	| length (x:y:xs) == 2 = check_cond p x y
	| otherwise = if not $ check_cond p x y then False
	                                        else check_sign p (y:xs)


check_all_skip_cases :: [Int] -> Bool
check_all_skip_cases line = or [check_skip skip line | skip <- [0..len - 1]]
	where len = length line

check_skip :: Int -> [Int] -> Bool
check_skip skip xs
	| skip == length xs - 1 = check $ init xs
	| skip == length xs - 2 = check $ take (length xs - 2) xs ++ [last xs]
check_skip 0 (x:y:z:xs) = check_skip_start 0 y z (x:y:z:xs)
check_skip 1 (x:y:z:xs) = check_skip_start 1 x z (x:y:z:xs)
check_skip skip (x:y:xs) = check_skip_start skip x y (x:y:xs)

check_skip_start :: Int -> Int -> Int -> [Int] -> Bool
check_skip_start skip x y xs = if x > y then check_sign_skip skip (>) xs
                                        else check_sign_skip skip (<) xs

check_sign_skip :: Int -> (Int -> Int -> Bool) -> [Int] -> Bool
check_sign_skip 0 p (_:x:xs) = check_sign_skip (- 1) p (x:xs)
check_sign_skip 1 p (x:y:z:xs) = if not $ check_cond p x z then False
                                 else check_sign_skip 0 p (y:z:xs)
check_sign_skip skip p (x:y:xs)
	| skip < 0 = check_sign p (x:y:xs)
	| otherwise = if not $ check_cond p x y then False
	                                        else check_sign_skip (skip - 1) p (y:xs)

check_cond p x y = x `p` y && abs (x - y) <= 3
