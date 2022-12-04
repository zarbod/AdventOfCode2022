import System.IO
import Data.List.Split

toInt :: [String] -> [[Int]]
toInt xs = map (map (\a -> read a::Int)) . map (splitOn "-") $ xs

contains :: [[Int]] -> Bool
contains (x:y:xs) = (check x y) || (check y x)
  where check a b = (head a <= head b && last a >= last b)

overlaps :: [[Int]] -> Bool
overlaps (x:y:xs) = (check x y) || (check y x)
  where check a b = (head a <= head b && last a >= head b)

main :: IO ()
main = do
  input <- openFile "input.txt" ReadMode
  contents <- hGetContents input
  let pairs = map (splitOn ",") . filter(/= "") . splitOn "\n" $ contents
  putStrLn . show . length . filter (==True) . map (contains) . map (toInt) $ pairs -- part 1
  putStrLn . show . length . filter (==True) . map (overlaps) . map (toInt) $ pairs -- part 2
  hClose input
