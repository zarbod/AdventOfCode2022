import System.IO
import Data.List.Split

maxList :: [[Int]] -> Int
maxList xs = foldl (\acc x -> if acc > sum x then acc else sum x) 0 xs

main :: IO ()
main = do
  input <- openFile "input.txt" ReadMode
  food <- hGetContents input
  let elves = splitOn "\n\n" food
  let calories = map (splitOn "\n") elves
  let calories' = map (\xs -> filter (/="") xs) calories
  let intCalories = map (map (\x -> read x::Int)) calories'
  let best = maxList intCalories
  let secondBest = maxList $ filter (\xs -> sum xs /= best) intCalories
  let thirdBest = maxList $ filter (\xs -> sum xs /= secondBest) $
        filter (\xs -> sum xs /= best) intCalories
  putStrLn $ show (best + secondBest + thirdBest)
  hClose input
