-- call the program using `main 1000` to start searching from step 1000
-- you get the idea

import Data.List(sort, group)
import Debug.Trace(trace)
import System.Environment(getArgs)

debug = flip trace

type Vec2i = (Int, Int)
type Queue = [Vec2i]
data Quad = None | TopLeft | TopRight | BottomLeft | BottomRight
            deriving (Eq, Ord, Show)

main = do
	task0 "../input/example.txt" 11 7
	task0 "../input/input.txt" 101 103
	task1 "../input/input.txt" 101 103

task0 path mapW mapH = do
	input <- readFile path
	let robotList
		= map (\(a:b:c:d:[]) -> ((a, b), (c, d)))
		$ map (map read) 
		$ map words
		$ lines
		$ filter (\c -> (c >= '0' && c <= '9')
		         || c == '\n' || c == ' ' || c == '-')
		$ map (\c -> if c == ',' then ' ' else c) input :: [(Vec2i, Vec2i)]

	let quadList
		= map (getQuad mapW mapH . fst)
		$ (iterate (map (nextPos mapW mapH)) robotList) !! 100 :: [Quad]

	let result
		= foldr1 (*) $ map length $ group $ sort
		$ filter (\group -> group /= None) $ quadList :: Int

	print result
--	print quadList

task1 path mapW mapH = do
	input <- readFile path
	args <- getArgs
	let searchStart = read $ args !! 0

	let robotList
		= map (\(a:b:c:d:[]) -> ((a, b), (c, d)))
		$ map (map read) 
		$ map words
		$ lines
		$ filter (\c -> (c >= '0' && c <= '9')
		         || c == '\n' || c == ' ' || c == '-')
		$ map (\c -> if c == ',' then ' ' else c) input :: [(Vec2i, Vec2i)]

	let iterList = (iterate f (robotList, 0, 0))
	    f :: ([(Vec2i, Vec2i)], Int, Int) -> ([(Vec2i, Vec2i)], Int, Int)
	    f (robotList, i, _) = (rRobotList, i + 1, maxBlobSz) `debug` show (i, maxBlobSz)
	        where rRobotList = map (nextPos mapW mapH) robotList
	              maxBlobSz = if i < searchStart then 0
	                          else getMaxBlobSz charMap mapW mapH
	              charMap = toMap mapW mapH rRobotList

	let (rRobotList, i, n) = filter (\(_, _, n) -> n > 20) iterList !! 0
	putStrLn $ "index: " ++ show i
	putStrLn $ toMap mapW mapH rRobotList

toMap :: Int -> Int -> [(Vec2i, Vec2i)] -> [Char]
toMap mapW mapH robotList = botMap
	where posList = map fst robotList
	      posMap = [(x, y) | y <- [0 .. mapH - 1], x <- [0 .. mapW]]
	      botMap = map (\(x, y) -> if x == mapW then '\n'
	                               else if elem (x, y) posList then '1'
	                               else '0') posMap

getMaxBlobSz :: [Char] -> Int -> Int -> Int
getMaxBlobSz charMap mapW mapH
	= snd $ foldr f ([], 0) posList
	where f :: Vec2i -> (Queue, Int) -> (Queue, Int)
	      f (x, y) (queue, acc)
	          = if charMap !! (y * (mapW + 1) + x) == '0' then (queue, acc)
	            else if elem (x, y) queue then (queue, acc)
	            else let (rQueue, rN) = search charMap mapW mapH [(x, y)] [] 0
	                 in (rQueue ++ queue, max acc rN)
	      posList = [(x, y) | y <- [0 .. mapH - 1], x <- [0 .. mapW - 1]]

search :: [Char] -> Int -> Int -> Queue -> Queue -> Int -> (Queue, Int)
search _       _    _    []             rest i = (rest, i)
search charMap mapW mapH ((x, y):queue) rest i
	= search charMap mapW mapH nxQueue ((x, y):rest) (i + 1)
	where dirs = [(x, y - 1), (x, y + 1), (x - 1, y), (x + 1, y)]
	      nxQueue = foldr f queue dirs

	      f :: Vec2i -> Queue -> Queue
	      f (x, y) acc = if cond x y then ((x, y):acc)
	                                 else acc

	      cond :: Int -> Int -> Bool
	      cond x y = x >= 0 && y >= 0 && x < mapW && y < mapH
	              && not (elem (x, y) queue) && not (elem (x, y) rest)
	              && (charMap !! (y * (mapW + 1) + x)) == '1'

nextPos :: Int -> Int -> (Vec2i, Vec2i) -> (Vec2i, Vec2i)
nextPos mapW mapH ((x, y), (vx, vy)) = (resultVec, (vx, vy))
	where resultVec = let (nxx, nxy) = (x + vx, y + vy)
	                      rx = if nxx >= mapW then nxx - mapW
	                           else if nxx < 0 then nxx + mapW
	                           else nxx
	                      ry = if nxy >= mapH then nxy - mapH
	                           else if nxy < 0 then nxy + mapH
	                           else nxy
	                  in (rx, ry)

getQuad :: Int -> Int -> Vec2i -> Quad
getQuad mapW mapH (x, y) = if x == mapWd || y == mapHd then None
                      else if x <  mapWd && y <  mapHd then TopLeft
                      else if x >  mapWd && y <  mapHd then TopRight
                      else if x <  mapWd && y >  mapHd then BottomLeft
                      else                                  BottomRight
	where mapWd = mapW `div` 2
	      mapHd = mapH `div` 2