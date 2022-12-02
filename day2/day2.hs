import System.IO
import Data.List.Split

points :: (Int, Int, Int, Int, Int, Int) -> Int -> [String] -> Int
points (x1, x2, x3, x4, x5, x6) t xs
  | b == "X"  = x1 + if a == "A" then x4 else if a == "B" then x5 else x6
  | b == c    = x2 + if a == "A" then x6 else if a == "B" then x4 else x5
  | otherwise = x3 + if a == "A" then x5 else if a == "B" then x6 else x4
  where a = head xs
        b = last xs
        c = if t == 0 then "Y" else "Z"

main :: IO ()
main = do
  input <- openFile "input.txt" ReadMode
  contents <- hGetContents input
  let rps = filter (\x -> last x /= "") . map (splitOn " ") . splitOn "\n" $ contents
  putStrLn $ show (sum . map (points (1, 2, 3, 3, 0, 6) 0) $ rps) -- part 1
  putStrLn $ show (sum . map (points (0, 6, 3, 3, 1, 2) 1) $ rps) -- part 2
  hClose input
