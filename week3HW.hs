module Golf where

import Data.List (sort, group, maximumBy, transpose, intercalate)
import Data.Ord (comparing)

-- Part One -- 
skips :: [a] -> [[a]]
skips xs = 
    map (f xs) [1 .. length xs] 


f :: [a] -> Int -> [a]
f xs n = concat $ map (flip skip n) (zip [1..] xs)

skip :: (Int, a) -> Int -> [a]
skip (i, elem) n = 
    if mod i n == 0 then
        [elem]
    else 
        []

-- Part Two --

localMaxima :: [Integer] -> [Integer]
localMaxima (a : r@(b : c : _))
    | b > a && b > c = b : localMaxima r
    | True           = localMaxima r
localMaxima _        = []


-- Part Three --


histogram :: [Int] -> String
histogram xs = 
    let 
        lst = map (flip replicate "*") $ map (count xs) [0..9]
        longest = length $ maximumBy (comparing length) lst
    in 
        (join $ 
            map 
                (join . (["\n"]++)) 
                (reverse $ transpose $ map (fill longest) lst)
        )
        ++ "\n=========="
        ++ "\n0123456789\n"

count :: [Int] -> Int -> Int
count xs x = (length . filter (==x)) xs


fill :: Int -> [String] -> [String]
fill x xs = xs ++ replicate (x - length xs) " "   


join :: [String] -> String
join = intercalate ""