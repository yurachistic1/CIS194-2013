toDigits :: Integer -> [Integer]
toDigits n
    | n <= 0        = []
    | otherwise     = toDigits (div n 10) ++ [(mod n 10)]


toDigitsRev :: Integer -> [Integer]
toDigitsRev n
    | n <= 0        = []
    | otherwise     = mod n 10 : toDigitsRev (div n 10)


doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther (x : y : rest) = x : y * 2 : doubleEveryOther rest
doubleEveryOther (x : []) = [x]
doubleEveryOther [] = []


sumDigits :: [Integer] -> Integer
sumDigits (x : rest) = x + sumDigits rest
sumDigits [] = 0


validate :: Integer -> Bool
validate n = mod ((sumDigits 
                    .concat 
                    .mapToDigits 
                    .doubleEveryOther 
                    .toDigitsRev) n) 10 == 0


mapToDigits :: [Integer] -> [[Integer]]
mapToDigits []          = []
mapToDigits (x : xs)    = toDigits x : mapToDigits xs