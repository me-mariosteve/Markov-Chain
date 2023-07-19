module Markov where


import qualified Data.Map as Map
import Data.Map ((!), Map)


count :: (Ord k,Integral n) => [k] -> Map k n
count = foldl updateCount Map.empty

updateCount :: (Ord k,Integral n) => Map k n -> k -> Map k n
updateCount counters key = Map.insert key (count+1) counters
  where count = maybe 0 id $ Map.lookup key counters


getSubSeqs :: [a] -> [[a]]
getSubSeqs [] = []
getSubSeqs (x:xs) = scanl (\a b -> a++[b]) [x] xs ++ getSubSeqs xs

getSubSeqsWithLength :: (Int -> Bool) -> [a] -> [[a]]
getSubSeqsWithLength flen xs = [ (take i . drop j) xs | i <- filter flen [1 .. len], j <- [0 .. len - i] ]
  where len = length xs


calcFreqs :: (Ord k, Integral c, Fractional f) => Map k c -> Map k f
calcFreqs counts = Map.map ((/s) . fromIntegral) counts
  where s = (fromIntegral . sum) counts

{--
 - For a map of n items (k,v), with vi <= v(i+1):
 - take ki such that v0+v1+...+vi >= v0+v1+...+vj for j from 1 to i
 -}
fromFreq :: (Ord k, Fractional f, Ord f) => f -> Map k f -> k
fromFreq f fs = Map.keys fs !! ( (+ (-1)) . length . takeWhile (<=f) . scanl (\acc (k,f) -> acc+f) 0 $ Map.toAscList fs )

