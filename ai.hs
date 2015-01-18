import Data.Clustering.Hierarchical

data Point = Point [Double] deriving Show

main = do
  contents <- readFile "test.txt"
  let list = parse contents
  let points2 = map Point list

  let points = map Point [[0,0], [1,0], [0,1], [1,1], [7,5], [9,6], [8,7]]
  let clusters = dendrogram SingleLinkage points dist
  
  printCluster clusters 6

  let resp = "holis"
  writeFile "result.txt" resp

dist :: Point -> Point -> Distance
dist (Point a) (Point b) = sqrt $ sum $ map (^2) $ zipWith (-) a b

printCluster :: Dendrogram Point -> Double -> IO ()
printCluster clusters cut = do
  let es = map elements $ clusters `cutAt` cut
  mapM_ print es

parse :: String -> [[Double]]
parse a = map (map read . words) (lines a)