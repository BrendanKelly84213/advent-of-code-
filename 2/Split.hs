module Split 
(
         split
) where

split :: Char -> String -> [String]
split _ "" = [] -- empty list
split tok str = helper str "" -- start helper with empty current word
    where helper :: String -> String -> [String]
          -- when the entire string is consumed
          helper "" ""      = [] -- if no current word, append nothing
          helper "" current = [current] -- if current word, append this to the list
          -- otherwise
          helper (x:xs) current
              | x == tok              = current : helper xs "" -- but skip on whitespaces
              | otherwise             = helper xs (current ++ [x]) -- if no seperator, just continue building up the current word
