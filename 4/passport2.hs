import Data.List
import Data.List.Split
import Data.Char


type Passport = [(String, String)]

-- cid plays no part at all in the solution
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

-- Turn input string into a list of paragraphs 
paragraphs :: String -> [String]
paragraphs = map (\str -> map repl str) . splitOn "\n\n"
        where repl '\n' = ' ' 
              repl c = c

-- Input string to list of passports 
fandv :: String -> [Passport]
fandv = map fv . paragraphs 
        where fv = map fieldval . words 
              fieldval x = (head $ splitOn ":" x, head $ tail $ splitOn ":" x)

-- Check if one list is a sublist of another
subList :: (Eq a) => [a] -> [a] -> Bool
subList xs ys = flip all xs $ flip elem ys

-- Check if all the fields are present in a passport 
isPresent :: Passport -> Bool
isPresent = subList valid . map fst  

-- Check if all fields are valid 
isValid :: Passport -> Bool
isValid = all isFValid 
        where isFValid (f, v) -- Check if a given field is valid
                | f == "byr" = vi >= 1920 && vi <= 2002
                | f == "iyr" = vi >= 2010 && vi <= 2020 
                | f == "eyr" = vi >= 2020 && vi <= 2030 
                | f == "hgt" = 
                        let t = head $ reverse v 
                        in if t == 'm' 
                                then vi >= 150 && vi <= 193 
                        else if t == 'n' then vi >= 59 && vi <= 76 
                        else False
                | f == "hcl" = 
                        if not (elem '#' v) then False 
                        else 
                        let 
                                chars = filter (/= '#') v; 
                                hex = ['0'..'9'] ++ ['a'..'f']
                        in subList chars hex && length chars == 6
                | f == "ecl" = 
                        let ecls = words "amb blu brn gry grn hzl oth" 
                        in elem v ecls
                | f == "pid" = length v == 9 
                | f == "cid" = True 
                | otherwise = False 
                where vi = (read $ filter isDigit v) :: Int

-- The solution
solution :: String -> Int 
solution = sum . map c . map f . fandv 
        where c True = 1
              c False = 0
              f x = isPresent x && isValid x 

main = do 
        print . solution =<< readFile "input.txt"
