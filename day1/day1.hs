import System.IO
import Data.List.Split

sort :: [Int] -> [Int]
sort [] = []
sort (x:xs) =
  let smaller = sort [a | a <- xs, a < x]
      larger = sort [a | a <- xs, a >= x]
      in larger ++ [x] ++ smaller

main :: IO ()
main = do
  input <- openFile "input.txt" ReadMode
  food <- hGetContents input
  let elves = splitOn "\n\n" food
  let calories = map (splitOn "\n") elves
  let calories' = map (\xs -> filter (/="") xs) calories
  let intCalories = map (map (\x -> read x::Int)) calories'
  let sums = foldr (\x acc -> (sum x):acc) [] intCalories
  putStrLn $ show . sum . take 3 . sort $ sums
  hClose input
