module Utility.Menu where

highlight :: (Eq a) => [[a]] -> [([a], a, [a])]
highlight xs = reverse . map snd $ foldl combine [] xs

combine :: (Eq a) => [(a, ([a], a, [a]))] -> [a] -> [(a, ([a], a, [a]))]
combine old xs = combine' old [] xs
    where combine' old acc (x:xs) = case x `lookup` old of
                                        Nothing  -> (x, (reverse acc, x, xs)) : old
                                        (Just _) -> combine' old (x:acc) xs
          combine' _   _   []     = []

showHighlighted :: (Show a) => ([a], a, [a]) -> String
showHighlighted (pre, x, post) = show pre ++ "<" ++ show x ++ ">" ++ show post

