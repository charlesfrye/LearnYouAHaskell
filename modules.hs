import Data.List
import Data.Char

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

search :: (Eq a) => [a] -> [a] -> Bool
search needle haystack =
    let nlen = length needle
    in foldl (\acc x -> 
    if take nlen x == needle then True else acc) False (tails haystack)

on' :: (b -> b -> c) -> (a->b) -> a -> a -> c
f `on'` g = \x y -> f (g x) (g y)

encode :: Int -> String -> String
encode shift msg =
    let ords = map ord msg
        shifted = map (+ shift) ords
    in map chr shifted

encode' :: Int -> String -> String
encode' shift msg =
    map (chr . (+ shift) . ord) msg

phoneBook =
    [("betty","555-2938")
    ,("bonnie","452-2928")
    ,("betty","1800HOTLINEBLING")]

findKey :: (Eq k) => k -> [(k,v)] -> v
findKey key xs = snd . head . filter (\(k,v) -> key == k) $ xs

findKey' :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey' key [] = Nothing
findKey' key ((k,v):xs) = if key == k
				then Just v
				else findKey' key xs

findKey'' key = foldl (\acc (k,v) -> if key == k then Just v else acc) Nothing
