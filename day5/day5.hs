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

move :: Int -> Int -> [String] -> [String]
move x y xs = foldl (\acc (t1, t2) -> if t2 == y 
  then acc ++ ([box]:[t1]) else acc ++ [t1]) [[]] arr 
  where arr = (zip xs [1..(length xs)])
        box = head $ xs !! (x - 1)

main :: IO ()
main = do
    input <- openFile "d5.txt" ReadMode
    contents <- hGetContents input
    let mat = filter (/="") . splitOn "\n" $ contents
    let num = ((length . head $ mat) + 1 ) `div` 4
    let crates = init . takeWhile ((==(4 * num - 1)) . length) $ mat
    let finStack = map (takeEveryN 3 1) $ map (trunc 4) crates
    let stacks = foldl (\acc x -> parse x acc) (listOfNLists(num)) finStack 
    print $ move 2 1 stacks
    print stacks 
    hClose input
