import Data.List(sort, group)
import Debug.Trace(trace)

debug = flip trace

type Vec2i = (Int, Int)
data Quad = None | TopLeft | TopRight | BottomLeft | BottomRight
            deriving (Eq, Ord, Show)

main = do
	task0 "../input/example.txt" 11 7
	task0 "../input/input.txt" 101 103

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
		$ fst
		$ (iterate f (robotList, 0)) !! 100 :: [Quad]
	    
	    f :: ([(Vec2i, Vec2i)], Int) -> ([(Vec2i, Vec2i)], Int)
	    f (robotList, i) = (map (nextPos mapW mapH) robotList, i + 1) `debug` (toMap mapW mapH robotList ++ show i)

	let result
		= foldr1 (*) $ map length $ group $ sort
		$ filter (\group -> group /= None) $ quadList :: Int

	print result
--	print quadList

toMap :: Int -> Int -> [(Vec2i, Vec2i)] -> [Char]
toMap mapW mapH robotList = botMap
	where posList = map fst robotList
	      posMap = [(x, y) | y <- [0 .. mapH - 1], x <- [0 .. mapW]]
	      botMap = map (\(x, y) -> if x == mapW then '\n'
	                               else if elem (x, y) posList then '1'
	                               else '0') posMap

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