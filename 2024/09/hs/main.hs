import Data.List(elemIndex)

main = do
	task0 "../input/example.txt"
--	task0 "../input/input.txt"
	task1 "../input/example.txt"
	task1 "../input/input.txt"

task0 path = do
	input <- readFile path
	let diskMap  = map (\c -> read [c]) input :: [Int]
	let blockMap = toBlockMap SpaceFile 0 diskMap :: [Int]
	let movedMap = toMovedMap $ reverse blockMap :: [Int]
	print $ calc 0 movedMap

task1 path = do
	input <- readFile path
	let diskMap  = map (\c -> read [c]) input :: [Int]
	let blockMap = toBlockMap SpaceFile 0 diskMap :: [Int]
	let movedMap = toMovedMap1 [] $ reverse blockMap :: [Int]
	print $ calc 0 $ map (\x -> if x == -1 then 0 else x) movedMap

data Space = SpaceFile | SpaceFree

toBlockMap :: Space -> Int -> [Int] -> [Int]
toBlockMap _ _ [] = []
toBlockMap SpaceFile n (x:diskMap)
	= (take x $ repeat $ n) ++ toBlockMap SpaceFree n diskMap
toBlockMap SpaceFree n (x:diskMap)
	= (take x $ repeat (-1)) ++ toBlockMap SpaceFile (n + 1) diskMap

toMovedMap :: [Int] -> [Int]
toMovedMap (-1:blockMapReversed) = toMovedMap blockMapReversed
toMovedMap (x:blockMapReversed)
	= case getEmptySpotI $ reverse blockMapReversed of
		Nothing -> reverse $ x:blockMapReversed
		Just i  -> toMovedMap $ (take i blockMapReversed)
	                         ++ (x : drop (i + 1) blockMapReversed)

getEmptySpotI :: [Int] -> Maybe Int
getEmptySpotI xs = case i of
	Just i -> Just $ length xs - i - 1
	Nothing -> Nothing
	where i = elemIndex (-1) xs

calc :: Int -> [Int] -> Int
calc _ [] = 0
calc i (x:xs) = i * x + calc (i + 1) xs


toMovedMap1 :: [Int] -> [Int] -> [Int]
toMovedMap1 rest [] = rest
toMovedMap1 rest (-1:blockMapReversed) = toMovedMap1 (-1:rest) blockMapReversed
toMovedMap1 rest (x:blockMapReversed) = case i of
	Nothing -> toMovedMap1 (take sz (repeat x) ++ rest)
	                       (drop (sz - 1) blockMapReversed)
	Just i  -> let _ls = (take (i - sz + 1) blockMapReversed)
	                  ++ (take sz $ repeat x)
	                  ++ (drop (i + 1) blockMapReversed) :: [Int]
	           in toMovedMap1 rest $ (take sz $ repeat (-1))
	                              ++ (drop (sz - 1) _ls)
	where sz = chunkSz x (x:blockMapReversed) :: Int
	      i = case emptyChunkI of
	          Nothing -> Nothing
	          Just i -> Just $ length blockMapReversed - i - 1 :: Maybe Int
	      emptyChunkI = getEmptyChunkI sz 0 $ reverse blockMapReversed

getEmptyChunkI :: Int -> Int -> [Int] -> Maybe Int
getEmptyChunkI _ _ [] = Nothing
getEmptyChunkI sz i (-1:xs)
	| n >= sz = Just i
	| otherwise = getEmptyChunkI sz (i + n) (drop (n - 1) xs)
	where n = chunkSz (-1) (-1:xs) :: Int
getEmptyChunkI sz i (x:xs) = getEmptyChunkI sz (i + 1) xs

chunkSz :: Int -> [Int] -> Int
chunkSz _ [] = 0
chunkSz n (x:xs)
	| x /= n = 0
	| otherwise = 1 + chunkSz n xs
