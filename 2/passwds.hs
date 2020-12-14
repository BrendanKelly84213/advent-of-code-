import Split 

test = ["1-3 a: abcde","1-3 b: cdefg","2-9 c: ccccccccc"]

readInt :: String -> Int 
readInt = read

-- Cut the data down into an array of strings 
tokens :: String -> [String]
tokens = words . map (\a -> if or [a == '-', a ==':'] then ' ' else a) 

-- Turn an array of strings of numbers into an array of ints
toInt :: [String] -> [Int]
toInt = map readInt 

-- Parse and get Data from the given string

-- Get the upper and lower bounds 
bounds :: String -> [Int]
bounds = toInt . take 2 . tokens

-- The password of the data
password :: String -> String
password = (!! 3) . tokens

-- The char which is to be constrained
condChar :: String -> Char
condChar = (!! 0) . (!! 2) . tokens

-- Get number of times an element occurs in a list 
repititions :: (Eq a) => a -> [a] -> Int
repititions e = length . filter (\a -> a == e) 

-- Check if the condition is met
checkCond :: String -> Bool
checkCond str =
        let c = condChar str
            bs = bounds str
            n = repititions c $ password str
        in and [n >= bs !! 0, n <= bs !! 1]

xor :: Bool -> Bool -> Bool
xor True a = not a
xor False a = a

checkCond2 :: String -> Bool
checkCond2 str = 
        let c = condChar str
            is = bounds str -- Bounds are now indices
            pass = password str
            f n = ((!!) pass $ ((!! n) is) - 1) == c -- Get the nth index of the indices, then check if that index of the password is the char 
            low = f 0
            high = f 1
        in xor low high 

-- The number of passwords that satisfies the condition 
numTrue :: [String] -> Int 
numTrue = length . filter (\a -> a) . map checkCond

-- The number of passwords that satisfies the condition in the second phase of the thing 
numTrue2 :: [String] -> Int 
numTrue2 = length . filter (\a -> a) . map checkCond2

main :: IO ()
main = do
        print . numTrue2 . split '\n' =<< readFile "input.txt"
