import Data.List (sortBy)
import Data.Ratio (numerator)

parseFile :: String -> [[Integer]]
parseFile input =
  map (map (\y -> read y :: Integer) . words) (lines input)

mapWithIndex :: (a -> Int -> b) -> [a] -> [b]
mapWithIndex c l = zipWith c l [0 .. ]

-- a + bx + cx^2 + dx^3
-- 0^0 + 0^1 + 0^2 + 0^3
-- 1^0 + 1^1 + 1^2 + 1^3

getMatrixLine :: Int -> Int -> Integer -> ([Rational], Rational)
getMatrixLine len ind solution = (map (\v -> toRational (ind' ^ toInteger v)) [0..(len-1)], toRational solution)
  where ind' = toInteger ind

countWhile :: (a -> Bool) -> [a] -> Int
countWhile _ [] =  0
countWhile p (x:xs)
  | p x       =  1 + countWhile p xs
  | otherwise =  0

countZeros :: [Rational] -> Int
countZeros = countWhile (\r -> numerator r == 0)

sortWith :: Ord b => (a -> b) -> [a] -> [a]
sortWith c = sortBy (\a b -> c a `compare` c b)

sortAugMatrix :: [([Rational], Rational)] -> [([Rational], Rational)]
sortAugMatrix = sortWith (\(a, _) -> countZeros a)

applyToSecondHalf :: ([a] -> [a]) -> Int -> [a] -> [a]
applyToSecondHalf c i l = l' ++ c l''
  where (l', l'') = splitAt i l

applyToFirst :: (a -> a) -> [a] -> [a]
applyToFirst _ [] = []
applyToFirst c (f:r) = c f : r

simplifyAugMatrixLine :: ([Rational], Rational) -> ([Rational], Rational)
simplifyAugMatrixLine l = if z >= len then l else multiplyAugMatrixLine (recip (fst l !! z)) l
  where z = countZeros (fst l)
        len = length $ fst l

reduceRowEchelonAugMatrix :: [([Rational], Rational)] -> [([Rational], Rational)]
reduceRowEchelonAugMatrix m = map simplifyAugMatrixLine $ reverse $ foldl (\m' i -> applyToSecondHalf (substractFirstFromAugMatrixLines (len - 1 - i)) i m') (reverse m) [0..(len - 1)]
  where len = length m

rowEcholenAugMatrix :: [([Rational], Rational)] -> [([Rational], Rational)]
rowEcholenAugMatrix m = foldl (\m' i -> applyToSecondHalf (substractFirstFromAugMatrixLines i) i m') m [0..(length m)]

multiplyAugMatrixLine :: Rational -> ([Rational], Rational) -> ([Rational], Rational)
multiplyAugMatrixLine f (m, s) = (map (* f) m, s * f)

addAugMatrixLine :: ([Rational], Rational) -> ([Rational], Rational) -> ([Rational], Rational)
addAugMatrixLine (a', a'') (b', b'') = (zipWith (+) a' b', a'' + b'')

substractFirstFromAugMatrixLines :: Int -> [([Rational], Rational)] -> [([Rational], Rational)]
substractFirstFromAugMatrixLines _ [] = []
substractFirstFromAugMatrixLines i (f:r) = f:map (\r' -> addAugMatrixLine r' (multiplyAugMatrixLine (negate ((fst r' !! i) / a)) f)) r
  where a = fst f !! i

getAugMatrix :: [Integer] -> [([Rational], Rational)]
getAugMatrix v = mapWithIndex (flip (getMatrixLine l)) v
  where l = length v

augMatrixToFactors :: [([Rational], Rational)] -> [Rational]
augMatrixToFactors = map snd

getPreviousValueForFactors :: [Rational] -> Rational
getPreviousValueForFactors factors = sum $ mapWithIndex (\factor i -> factor * (x ^ i)) factors
  where x = toRational (-1)

solve v = sum $ map (numerator . getPreviousValueForFactors . augMatrixToFactors . reduceRowEchelonAugMatrix . rowEcholenAugMatrix . getAugMatrix) v
-- solve _ = simplifyAugMatrixLine ([toRational 0, toRational 5], toRational 4)
-- solve v = getNextValueForFactors . augMatrixToFactors . reduceRowEchelonAugMatrix . rowEcholenAugMatrix . getAugMatrix $ (v !! 1)
-- solve v = getAugMatrix $ (v !! 4)

main = do
    input <- getContents
    print $ solve $ parseFile input