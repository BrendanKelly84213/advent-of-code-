import Data.Char
import Data.Bits
import Numeric
import Split

-- Given data for testing
test :: String
test = "..##.......\n\
      \#...#...#..\n\ 
      \.#....#..#.\n\
      \..#.#...#.#\n\
      \.#...##..#.\n\
      \..#.##.....\n\
      \.#.#.#....#\n\
      \.#........#\n\
      \#.##...#...\n\
      \#...##....#\n\
      \.#..#...#.#\n"

-- Given slopes from part 2 
slopes :: [(Int,Int)]
slopes = [(1,1), (3,1), (5,1), (7,1), (1,2)]

-- Number of hits given slope of (r,d) 
hits :: (Int,Int) -> [String] -> Int
hits (r,d) = sum . map f . filter (\t -> (fst t) `mod` d == 0) . zip [0,1..] -- We only visit strings which are multiples of d 
        where f :: (Int,String) -> Int
              f t = if c == '#' then 1 else 0 
                where 
                      str = snd t -- The string at whatever y coord we're at 
                      y = (fst t) `div` d -- Kinda stupid but works 
                      x = r*y `mod` (length str) -- Patterns repeat to the right forever (modulus)
                      c = str !! x


-- A very difficult to describe function,
-- Designed for the purpose appplying each slope to hits 
-- Maybe refactor later
run _ [] _ = []
run _ _ [] = []
run f (x:xs) ys = (f x ys):(run f xs ys)

main :: IO()
main = do 
        print . product . run hits slopes . split '\n' =<< readFile "input.txt" 
