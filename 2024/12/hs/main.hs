import Debug.Trace(trace)
import Data.List(sortBy)
import qualified Data.Set as Set

type Vec2i = (Int, Int)
type Checked = Set.Set Vec2i

main = do
	task0 "../input/example.txt"
--	task0 "../input/input.txt"
	task1 "../input/example.txt"
	task1 "../input/input.txt"

task0 path = do
	input <- readFile path
	let lineList = lines input
	let w = length $ lineList !! 0
	let h = length lineList

	let plotMap = filter (\c -> c /= '\n' && c /= '\r') input 
	let posList = [(x, y) | y <- [0 .. h - 1], x <- [0 .. w - 1]] :: [Vec2i]
	let plotList = zip plotMap posList :: [(Char, Vec2i)]

	let result = foldr f (Set.empty, 0) plotList :: (Checked, Int)
	    f :: (Char, Vec2i) -> (Checked, Int) -> (Checked, Int)
	    f (c, (x, y)) (checked, acc)
	        = let (rChecked, rN, rSides) = if (Set.notMember (x, y) checked)
	              then getRegionRes plotMap (w, h) checked c (x, y)
	              else (Set.empty, 0, 0)
	          in (Set.union rChecked checked, acc + (rN * rSides))

	print $ snd result

task1 path = do
	input <- readFile path
	let lineList = lines input
	let w = length $ lineList !! 0
	let h = length lineList

	let plotMap = filter (\c -> c /= '\n' && c /= '\r') input
	let posList = [(x, y) | y <- [0 .. h - 1], x <- [0 .. w - 1]] :: [Vec2i]
	let plotList = zip plotMap posList :: [(Char, Vec2i)]

	let result = foldr f (Set.empty, 0) plotList :: (Checked, Int)
	    f :: (Char, Vec2i) -> (Checked, Int) -> (Checked, Int)
	    f (c, (x, y)) (checked, acc)
	        = let (rChecked, rN, rSides) = if (Set.notMember (x, y) checked)
	              then getRegionRes1 plotMap (w, h) checked c (x, y)
	              else (Set.empty, 0, [])
	          in (Set.union rChecked checked, acc + (rN * g rSides))
	
	    g :: [(Side, (Int, Int))] -> Int
	    g list = (length $ hFilter hList []) + (length $ vFilter vList [])
	        where hSort (side0, (x0, y0)) (side1, (x1, y1)) =
	                  if side0 == side1 then
	                      if y0 == y1 then
	                          compare x0 x1
	                      else
	                          compare y0 y1
	                  else
	                      compare side0 side1
	              vSort (side0, (x0, y0)) (side1, (x1, y1)) =
	                  if side0 == side1 then
	                      if x0 == x1 then
	                          compare y0 y1
	                      else
	                          compare x0 x1
	                  else
	                      compare side0 side1

	              hList = sortBy hSort list :: [(Side, (Int, Int))]
	              vList = sortBy vSort list :: [(Side, (Int, Int))]
	              hListFiltered = hFilter hList

	print $ snd result

hFilter :: [(Side, (Int, Int))] -> [(Side, (Int, Int))] -> [(Side, (Int, Int))]
hFilter [(side, v)] rest
	| side /= SideLeft && side /= SideRight = (side, v):rest
	| otherwise = rest
hFilter [] rest = rest
hFilter ((side0, (x0, y0)):(side1, (x1, y1)):list) rest
	| side0 == SideLeft || side0 == SideRight
		|| (side0 == side1 && x0 + 1 == x1 && y0 == y1)
		= hFilter ((side1, (x1, y1)):list) rest
	| otherwise = hFilter ((side1, (x1, y1)):list) ((side0, (x0, y0)):rest)

vFilter :: [(Side, (Int, Int))] -> [(Side, (Int, Int))] -> [(Side, (Int, Int))]
vFilter [(side, v)] rest
	| side /= SideUp && side /= SideDown = (side, v):rest
	| otherwise = rest
vFilter [] rest = rest
vFilter ((side0, (x0, y0)):(side1, (x1, y1)):list) rest
	| side0 == SideUp || side0 == SideDown
		|| (side0 == side1 && x0 == x1 && y0 + 1 == y1)
		= vFilter ((side1, (x1, y1)):list) rest
	| otherwise = vFilter ((side1, (x1, y1)):list) ((side0, (x0, y0)):rest)

getRegionRes :: [Char] -> Vec2i -> Checked -> Char -> Vec2i -> (Checked,Int,Int)
getRegionRes plotMap (w, h) checked c (x, y) = res
	where cond :: Checked -> Int -> Int -> Bool
	      cond checked x y = (inRect w h x y) && (Set.notMember (x, y) checked)
	                                  && (plotMap !! (y * w + x) == c)
	      up    = (x, y - 1)
	      down  = (x, y + 1)
	      left  = (x - 1, y)
	      right = (x + 1, y)
	      dirs = [up, down, left, right]

	      res :: (Checked, Int, Int)
	      res = foldr f (Set.insert (x, y) checked, 1, _sideCount) dirs

	      f :: Vec2i -> (Checked, Int, Int) -> (Checked, Int, Int)
	      f (x, y) (checked, n, sides)
	          = let (rChecked, rN, rSides) = if cond checked x y
	                then getRegionRes plotMap (w, h) checked c (x, y)
	                else (Set.empty, 0, 0)
	            in (Set.union rChecked checked, rN + n, rSides + sides)

	      _sideCount = sideCount plotMap (w, h) (x, y) c

data Side = SideUp | SideDown | SideLeft | SideRight deriving (Eq, Ord, Show)
type RRegionRes1 = (Checked, Int, [(Side, (Int, Int))])

getRegionRes1 :: [Char] -> Vec2i -> Checked -> Char -> Vec2i -> RRegionRes1
getRegionRes1 plotMap (w, h) checked c (x, y) = res
	where cond :: Checked -> Int -> Int -> Bool
	      cond checked x y = (inRect w h x y) && (Set.notMember (x, y) checked)
	                                  && (plotMap !! (y * w + x) == c)
	      up    = (x, y - 1)
	      down  = (x, y + 1)
	      left  = (x - 1, y)
	      right = (x + 1, y)
	      dirs = [up, down, left, right]

	      res :: RRegionRes1
	      res = foldr f (Set.insert (x, y) checked, 1, _getSides) dirs

	      f :: Vec2i -> RRegionRes1 -> RRegionRes1
	      f (x, y) (checked, n, sides)
	          = let (rChecked, rN, rSides) = if cond checked x y
	                then getRegionRes1 plotMap (w, h) checked c (x, y)
	                else (Set.empty, 0, [])
	            in (Set.union rChecked checked, rN + n, rSides ++ sides)

	      _getSides = getSides plotMap (w, h) (x, y) c

inRect rw rh x y = x >= 0 && y >= 0 && x < rw && y < rh

sideCount :: [Char] -> Vec2i -> Vec2i -> Char -> Int
sideCount plotMap (w, h) (x, y) c
	= foldr (\(x, y) acc -> if cond x y then acc + 1 else acc) 0 dirs
	where cond x y = (not $ inRect w h x y) || (plotMap !! (y * w + x) /= c)
	      up    = (x, y - 1)
	      down  = (x, y + 1)
	      left  = (x - 1, y)
	      right = (x + 1, y)
	      dirs = [up, down, left, right]

getSides :: [Char] -> Vec2i -> Vec2i -> Char -> [(Side, (Int, Int))]
getSides plotMap (w, h) (x, y) c
	= foldr (\(side, (_x, _y)) acc -> if cond _x _y
	                                then (side, (x, y)):acc
	                                else acc) [] dirs
	where cond x y = (not $ inRect w h x y) || (plotMap !! (y * w + x) /= c)
	      up    = (x, y - 1)
	      down  = (x, y + 1)
	      left  = (x - 1, y)
	      right = (x + 1, y)
	      dirs = zip [SideUp, SideDown, SideLeft, SideRight]
	                 [up, down, left, right]

debug = flip trace
