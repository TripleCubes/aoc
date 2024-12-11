import Debug.Trace(trace)
import Data.Int(Int64)
import qualified Data.Map as Map

main = do
	task0 "../input/example.txt"
	task0 "../input/input.txt"

task0 path = do
	input <- readFile path
	let list = map read $ words input :: [Int64]
	let times = 75
	print $ snd $ blinkTimes (Map.fromList []) (times + 1) list

type Cache = Map.Map (Int64, Int64) Int64

blinkTimes :: Cache -> Int64 -> [Int64] -> (Cache, Int64)
blinkTimes cache 0 _ = (cache, 1)
blinkTimes cache times xs
	= foldr (\x (cache, n) -> let (rCache, rN) = f cache (times - 1) x
	                          in (rCache, rN + n)) (cache, 0) xs
	where f :: Cache -> Int64 -> Int64 -> (Cache, Int64)
	      f cache times x
	          | Map.member (times, x) cache = (cache, cache Map.! (times, x))
	          | otherwise = let (rCache, n) = blinkTimes cache times $ blinkN x
	                        in (Map.insert (times, x) n rCache, n)

combinePairs :: (Cache, Int64) -> (Cache, Int64) -> (Cache, Int64)
combinePairs (cache0, n0) (cache1, n1) = (Map.union cache0 cache1, n0 + n1)

blinkN :: Int64 -> [Int64]
blinkN n
	| n == 0    = [1]
	| digitCond = splitN numDigits n
	| otherwise = [n * 2024]
	where numDigits = getNumDigits n
	      digitCond = numDigits `mod` 2 == 0

getNumDigits :: Int64 -> Int64
getNumDigits x
	| x >= 0 && x <= 9 = 1
	| otherwise = 1 + getNumDigits (x `div` 10)

splitN :: Int64 -> Int64 -> [Int64]
splitN numDigits n = [n `div` divN, n `mod` divN]
	where divN = 10 ^ (numDigits `div` 2)

debug = flip trace
