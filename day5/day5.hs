import System.IO
import Data.List.Split

listOfNLists :: Int -> [String]
listOfNLists n = [[] | x <- [1..n]]

takeEveryN :: Int -> Int -> String -> String
takeEveryN _ _ [] = []
takeEveryN n start xs 
  | start == 0 = (head xs):(takeEveryN n 0 (drop n xs))
  | otherwise = takeEveryN n (0) (drop (start) xs) 

trunc :: Int -> String -> String
trunc _ [] = []
trunc n xs = take (n - 1) xs ++ trunc n (drop n xs) 

parse :: String -> [String] -> [String]
parse _ [] = []
parse [] _ = []
parse (x:xs) (y:ys)
  |x == ' ' = y:parse xs ys
  |otherwise = (y ++ [x]):parse xs ys

genStacks :: String -> [String]
genStacks contents = foldl (\acc x -> parse x acc) (listOfNLists(num)) finStack
  where mat = filter (/="") . splitOn "\n" $ contents
        num = ((length . head $ mat) + 1 ) `div` 4
        crates = init . takeWhile ((==(4 * num - 1)) . length) $ mat
        finStack = map (takeEveryN 3 1) $ map (trunc 4) crates

genMoves :: String -> [String]
genMoves xs = filter (/="from") . filter (/="to") . splitOn " " . drop 5 $ xs

-- above stuff is for parsing below stuff is for solving

move :: Int -> Int -> Int -> [String] -> [String]
move n x y xs = map (\(r, t) -> if (t == y) then box ++ r 
                    else if (t == x) then drop n r else r) arr
  where arr = zip xs [1..]
        box = take n $ xs !! (x - 1)

move_n :: Int -> Int -> Int -> [String] -> [String]
move_n 0 _ _ xs = xs
move_n n x y xs = move_n (n - 1) x y (move 1 x y xs)

step :: [String] -> [String] -> (Int -> Int -> Int -> [String] -> [String]) -> [String]
step (x:y:z:xs) stacks f = f (read x::Int) (read y::Int) (read z::Int) stacks

solve :: [[String]] -> [String] -> (Int -> Int -> Int -> [String] -> [String]) -> [String] 
solve [] stacks f = stacks 
solve (x:xs) stacks f = solve xs (step x stacks f) f

main :: IO ()
main = do
    input <- openFile "input.txt" ReadMode
    contents <- hGetContents input
    let stacks = genStacks contents
    let moves = map(genMoves). init . splitOn "\n" . last . splitOn "\n\n" $ contents
    let codeWord f = foldr (\x acc -> (head x):acc) "" (solve moves stacks f) -- part 1
    print $ codeWord (move_n) -- part 1
    print $ codeWord (move) -- part 2
    hClose input
