import Data.Map (Map, empty, fromList, findWithDefault)
import Data.List (unfoldr)
import Control.Monad (foldM)

data Direction = L | R deriving (Read, Show, Eq)

parseFile :: String -> ([Direction], Map String (String, String))
parseFile input =
  ( map (\a -> read [a] :: Direction) (head l),
    fromList $ map (\a -> (
      take 3 a,
      (
        take 3 (drop 7 a),
        take 3 (drop 12 a)
      )
    )) (drop 2 l) )
  where l = lines input

steps :: Map String (String, String) -> (String, Int) -> String -> [Direction] -> Either (String, Int) (String, Int)
steps m s e = foldM f s
  where
    f (v, c) d
      | v == e = Left (v, c)
      | otherwise = Right (if d == L then fst $ findWithDefault (v,v) v m else snd $ findWithDefault (v,v) v m , c+1)

solve :: ([Direction], Map String (String, String)) -> Either (String, Int) (String, Int)
solve (ds, m) = steps m ("AAA", 0) "ZZZ" (cycle ds)

main = do
    input <- getContents
    print $ solve $ parseFile input