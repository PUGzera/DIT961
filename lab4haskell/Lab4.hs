import Data.Map (Map)
import qualified Data.Map as M
import Graph as G
import Debug.Trace
import Route
import RouteGUI
import Data.PSQueue as PQ
import Data.Maybe
import System.Environment (getArgs)

shortestPath :: (Show b, Show a, Num b, Ord a, Ord b) => Graph a b -> a -> a -> Maybe ([a], b)
shortestPath g from to 
  | from == to = Just ([from], 0)
  | otherwise = Just $ (until (\(x:_) -> from == x) (\y@(x:_) -> 
  ((snd . fromJust $ Prelude.lookup x visited):y)) [to], fst . fromJust $ Prelude.lookup to visited)
  where
    visited = dijkstras g from

dijkstras :: (Show b, Show a, Num b, Ord a, Ord b) => Graph a b -> a -> [(a, (b, a))]
dijkstras g from = visitNeighbours (Prelude.foldr (\a pq -> 
  case compare from a of
    EQ -> PQ.insert a (0, a) pq
    _  -> PQ.insert a (20000000, a) pq) PQ.empty (vertices g)) g [] from

visitNeighbours :: (Show b, Show a, Num b, Ord a, Ord b) => PQ.PSQ a (b, a) -> Graph a b -> [(a, (b, a))] -> a -> [(a, (b, a))]
visitNeighbours pq g visited from
  | PQ.size pq < 3 = visitedList
  | otherwise      = visitNeighbours visitedPQ g visitedList (key minVal)
  where
    visitedPQ      = PQ.deleteMin $ visit pq (from, G.adj from g)
    minVal         = fromJust $ PQ.findMin visitedPQ
    visitedList    = (key minVal, prio minVal):visited
    
visit :: (Show b, Show a, Num b, Ord a, Ord b) => PQ.PSQ a (b, a) -> (a, [Edge a b]) -> PQ.PSQ a (b, a)
visit pq (x, xs) = (Prelude.foldr (\a pq' ->
   adjust (\v@(p, _) -> let (cost, _) = fromJust $ PQ.lookup x pq' in
                        case compare (cost + label a) p of
                          LT -> (cost + label a, src a)
                          _  -> v) (dst a) pq') pq xs)
                        
                      
testGraph :: Graph String Integer
testGraph = addEdge "a" "b" 2 
  $ addEdge "b" "c" 5 
  $ addEdge "c" "d" 3 
  $ addEdge "b" "d" 4
  $ Prelude.foldr addVertex G.empty ["a", "b", "c", "d"]


main :: IO ()
main = do
  [stopFile, linesFile, from, to] <- getArgs
  (_, _, graph) <- readGraph stopFile linesFile
  case shortestPath graph from to of
    Nothing -> putStrLn "No path!"
    Just (path, cost) -> do
      putStrLn $ show cost
      mapM_ putStrLn path

test :: IO ()
test = do
  let [stopFile, linesFile, from, to] = ["stops-gbg.txt", "lines-gbg.txt", "Angered", "Skogome"]
  (_, _, graph) <- readGraph stopFile linesFile
  case shortestPath graph from to of
    Nothing -> putStrLn "No path!"
    Just (path, cost) -> do
      putStrLn $ show cost
      mapM_ putStrLn path

startGUI :: FilePath -> FilePath -> IO ()
startGUI stopFile linesFile = do
  (stops, lines, graph) <- readGraph stopFile linesFile
  runGUI stops lines graph shortestPath

readGraph :: FilePath -> FilePath -> IO ([Stop], [LineTable], Graph Name Cost)
readGraph stopFile linesFile = do
  Right stops <- readStops stopFile 
  Right lines <- readLines linesFile
  let g = Prelude.foldr addLine (addVertices [n | Stop n _ <- stops] G.empty) lines
  return (stops, lines, g)
 where
  addLine (LineTable _ lineStops) g = case lineStops of
    [] -> g
    xs -> Prelude.foldr ($) g $ zipWith f xs (tail xs)  

  f x y = addEdge (stopName x) (stopName y) (time y)
