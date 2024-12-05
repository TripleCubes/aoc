import Data.List(elemIndex, sortBy)

main = do
	task_0 "../input/example.txt"
	task_0 "../input/input.txt"
	task_1 "../input/example.txt"
	task_1 "../input/input.txt"

task_0 path = do
	input <- readFile path
	let line_list = lines input
	    (rule_lines, page_lines) = split_lines line_list

	    rule_list = map (\(x:y:_) -> (read x, read y))
	              $ map (str_split '|') rule_lines :: [(Int, Int)]
	    update_list = map (map read)
	              $ map (str_split ',') page_lines :: [[Int]]
	
	    update_valid_list = map (update_valid rule_list) update_list :: [Bool]
	    mid_page_list = map (\xs -> let i = length xs `div` 2 in xs !! i)
	                        update_list :: [Int]
	    result = sum $ map (\(b, n) -> if b then n else 0)
	           $ zip update_valid_list mid_page_list
	print result

task_1 path = do
	input <- readFile path
	let line_list = lines input
	    (rule_lines, page_lines) = split_lines line_list

	    rule_list = map (\(x:y:_) -> (read x, read y))
	              $ map (str_split '|') rule_lines :: [(Int, Int)]
	    update_list = map (map read)
	              $ map (str_split ',') page_lines :: [[Int]]
	
	    invalid_list
	        = filter (not . (update_valid rule_list)) update_list :: [[Int]]
	    sorted_update_list
	        = map (page_list_sort rule_list) invalid_list :: [[Int]]
	    mid_page_list = map (\xs -> let i = length xs `div` 2 in xs !! i)
	                        sorted_update_list :: [Int]
	print $ sum mid_page_list


update_valid :: [(Int, Int)] -> [Int] -> Bool
update_valid rule_list page_list = num_false == 0
	where index_n_page_list
	          = [(i, page_list !! i) | i <- [0..length page_list - 1]]
	      f (i, n) = page_valid page_list rule_list 0 i n
	      num_false = length . filter (== False) . map f $ index_n_page_list

page_valid :: [Int] -> [(Int, Int)] -> Int -> Int -> Int -> Bool
page_valid [] _ _ _ _ = True
page_valid (x:xs) rule_list i ni n
	| follow_rules = page_valid xs rule_list (i + 1) ni n
	| otherwise = False
	where not_follow_rules = (must_after  rule_list n x && ni < i)
	                      || (must_before rule_list n x && ni > i)
	      follow_rules = not not_follow_rules

must_after :: [(Int, Int)] -> Int -> Int -> Bool
must_after rule_list a b = (length . filter (== (b, a)) $ rule_list) > 0

must_before :: [(Int, Int)] -> Int -> Int -> Bool
must_before rule_list a b = (length . filter (== (a, b)) $ rule_list) > 0

str_split :: Char -> String -> [String]
str_split _ "" = []
str_split c s
	| maybe_i == Nothing = [s]
	| otherwise = let (Just i) = maybe_i in (take i s)
	                           : (str_split c $ drop (i + 1) s)
	where maybe_i = elemIndex c s

split_lines :: [String] -> ([String], [String])
split_lines [] = ([], [])
split_lines (s:s_ls)
	| elem '|' s = (s:list_l, list_r)
	| otherwise = ([], s_ls)
	where (list_l, list_r) = split_lines s_ls


page_comp :: [(Int, Int)] -> Int -> Int -> Ordering
page_comp rule_list l r
	| must_before rule_list l r = LT
	| must_after rule_list l r = GT

page_list_sort :: [(Int, Int)] -> [Int] -> [Int]
page_list_sort rule_list page_list = sortBy (page_comp rule_list) page_list
