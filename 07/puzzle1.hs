import Data.Map (Map, fromListWith, toList)
import Data.List (sort, find, unfoldr, sortBy)
import Data.Maybe (fromMaybe)
import Data.Sequence (mapWithIndex, fromList)

data Card = CardA | CardK | CardQ | CardJ | CardT | Card9 | Card8 | Card7 | Card6 | Card5 | Card4 | Card3 | Card2 deriving (Show, Read, Eq, Ord)

data Hand = Hand { cards :: [Card],
                   bid :: Int} deriving (Show, Eq)

data HandType = FiveOfAKind | FourOfAKind | FullHouse | ThreeOfAKind | TwoPair | OnePair | HighCard deriving (Eq, Ord)

handType :: Hand -> HandType
handType h = case sort [v | (_,v) <- toList $ countValues $ cards h] of
  [5] -> FiveOfAKind
  [1,4] -> FourOfAKind
  [2,3] -> FullHouse
  [1,1,3] -> ThreeOfAKind
  [1,2,2] -> TwoPair
  [1,1,1,2] -> OnePair
  [1,1,1,1,1] -> HighCard
  _ -> error "something weird happened"

countValues :: forall a. Ord a => [a] -> Map a Int
countValues l = fromListWith (+) [ (v, 1) | v <- l]

compareCards :: [Card] -> [Card] -> Ordering
compareCards a b = fromMaybe
  EQ (find (/= EQ) (zipWith compare a b))

instance Ord Hand where
  compare :: Hand -> Hand -> Ordering
  a `compare` b = case (handType a) `compare` (handType b) of
    EQ -> compareCards (cards a) (cards b)
    v -> v

separateBy :: Eq a => a -> [a] -> [[a]]
separateBy chr = unfoldr sep where
  sep [] = Nothing
  sep l  = Just . fmap (drop 1) . break (== chr) $ l

parseHand :: String -> Hand
parseHand l = Hand { cards = [read ("Card" ++ [c]) :: Card | c <- head x],
                    bid = read (x !! 1) :: Int}
  where x = separateBy ' ' l

solve :: String -> Int
solve v = sum $ mapWithIndex (\index hand -> (index + 1) * (bid hand)) (fromList (sortBy (flip compare) [ parseHand l | l <- lines v]))

main = do
    input <- getContents
    print (solve input)