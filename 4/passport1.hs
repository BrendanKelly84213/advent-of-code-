import Data.List
import Data.List.Split

valid :: [String]
valid = [
         "byr", --(Birth Year)
         "iyr", --(Issue Year)
         "eyr", --(Expiration Year)
         "hgt", --(Height)
         "hcl", --(Hair Color)
         "ecl", --(Eye Color)
         "pid" --(Passport ID)
         ]

paragraphs :: String -> [String]
paragraphs = map (\str -> map repl str) . splitOn "\n\n"
        where repl '\n' = ' ' 
              repl c = c

fields :: String -> [String]
fields = map field . words   
        where field = head . splitOn ":"

isValid :: [String] -> Bool
isValid x = and $ map ($ x) $ map elem valid   

inputFields :: String -> [[String]]
inputFields = map fields . paragraphs

solution :: String -> Int 
solution = sum . map c . map isValid . inputFields 
        where c True = 1
              c False = 0

main = do 
        print . solution =<< readFile "input.txt"
