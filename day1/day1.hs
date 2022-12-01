import System.IO
import Data.List
import Data.List.Split

main :: IO ()
main = do
  input <- openFile "input.txt" ReadMode
  food <- hGetContents input
  let elves = splitOn "\n\n" food
  let calories = map (splitOn "\n") elves
  let calories' = map (\xs -> filter (/="") xs) calories
  let intCalories = map (map (\x -> read x::Int)) calories'
  let sums = foldr (\x acc -> (sum x):acc) [] intCalories
  putStrLn $ show . sum . take 3 . reverse . sort $ sums
  hClose input
