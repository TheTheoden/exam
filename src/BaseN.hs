module BaseN (BaseN(..)) where

data BaseN = BaseN Int [Int]  -- Base and list of digits

instance Show BaseN where
    show (BaseN base digits) = show base ++ ": " ++ concatMap show digits


instance Num BaseN where
    (BaseN base1 digits1) + (BaseN base2 digits2)
        | base1 /= base2 = error "Bases must be the same"
        | otherwise = fromBase10 base1 (toBase10 base1 digits1 + toBase10 base1 digits2)

    (BaseN base1 digits1) * (BaseN base2 digits2)
        | base1 /= base2 = error "Bases must be the same"
        | otherwise = fromBase10 base1 (toBase10 base1 digits1 * toBase10 base1 digits2)

    abs = id
    signum _ = 1
    fromInteger = error "only direct definition"
    negate = error "only positive vibes"

instance Eq BaseN where
    (BaseN base1 digits1) == (BaseN base2 digits2) = (toBase10 base1 digits1) == (toBase10 base2 digits2)

instance Ord BaseN where
    (BaseN base1 digits1) `compare` (BaseN base2 digits2) = compare (toBase10 base1 digits1) (toBase10 base2 digits2)

toBase10 :: Int -> [Int] -> Int
toBase10 base = foldl (\acc x -> acc * base + x) 0

fromBase10 :: Int -> Int -> BaseN
fromBase10 base num = BaseN base (convert num)
  where
    convert 0 = [0]
    convert n = helper n []
    helper 0 acc = acc
    helper n acc = helper (n `div` base) ((n `mod` base) : acc)