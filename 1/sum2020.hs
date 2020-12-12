test :: [Int]
test = [1721,979,366,299,675,1456]


sublists :: Int -> [a] -> [[a]]
sublists 0 _ = [[]]
sublists _ [] = []
sublists n (x:xs) =
        [x : subs | subs <- sublists (n-1) xs] 
        ++ sublists n xs    

findSum :: Int -> [Int] -> [[Int]]
findSum n = filter (\a -> sum a == 2020) . sublists n

main = do
        print . product . (!! 0) . findSum 3 . map readInt . words =<< readFile "input.txt"
        -- putStr "c\n"

readInt :: String -> Int
readInt = read
