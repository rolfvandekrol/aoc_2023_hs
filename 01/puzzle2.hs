import qualified Data.Map as Map
import qualified Data.List as List

m :: [(String, Int)]
m = [("1", 1),
     ("2", 2),
     ("3", 3),
     ("4", 4),
     ("5", 5),
     ("6", 6),
     ("7", 7),
     ("8", 8),
     ("9", 9),
     ("one", 1),
     ("two", 2),
     ("three", 3),
     ("four", 4),
     ("five", 5),
     ("six", 6),
     ("seven", 7),
     ("eight", 8),
     ("nine", 9)]

findDigitPrefix :: String -> Maybe Int
findDigitPrefix x = case List.find (\(prefix, value) -> prefix `List.isPrefixOf` x) m of
    Just (_, value) -> Just value
    Nothing -> Nothing

findDigitSuffix :: String -> Maybe Int
findDigitSuffix x = case List.find (\(prefix, value) -> prefix `List.isSuffixOf` x) m of
    Just (_, value) -> Just value
    Nothing -> Nothing

firstDigit :: String -> Int
firstDigit "" = error "not found"
firstDigit o = case findDigitPrefix o of
    Just d -> d
    Nothing -> firstDigit (tail o)

lastDigit :: String -> Int
lastDigit "" = error "not found"
lastDigit o = case findDigitSuffix o of
    Just d -> d
    Nothing -> lastDigit (init o)

solve :: String -> Int
solve v = sum [ 10 * firstDigit l + lastDigit l | l <- lines v]

main = do
    input <- getContents
    print (solve input)