import System.IO
import Data.List.Split

points :: [String] -> Int
points xs
  | b == "X"  = 1 + if a == "A" then 3 else if a == "B" then 0 else 6
  | b == "Y"  = 2 + if a == "A" then 6 else if a == "B" then 3 else 0
  | otherwise = 3 + if a == "A" then 0 else if a == "B" then 6 else 3
  where a = head xs
        b = last xs

main :: IO ()
main = do
  input <- openFile "input.txt" ReadMode
  contents <- hGetContents input
  let rps = filter (\x -> last x /= "") . map (splitOn " ") . splitOn "\n" $ contents
  putStrLn $ show (sum . map (points) $ rps)
  hClose input
