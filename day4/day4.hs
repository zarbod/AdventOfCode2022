import System.IO
import Data.List.Split

toInt :: [String] -> [[Int]]
toInt xs = map (map (\a -> read a::Int)) . map (splitOn "-") $ xs

contains :: ([Int] -> Int) -> [[Int]] -> Bool
contains f (x:y:xs) = (check x y) || (check y x)
  where check a b = (head a <= head b && last a >= f b)

main :: IO ()
main = do
  input <- openFile "input.txt" ReadMode
  contents <- hGetContents input
  let pairs = map (splitOn ",") . filter(/= "") . splitOn "\n" $ contents
  putStrLn . show . length . filter (==True) . map (contains last) . map (toInt) $ pairs -- part 1
  putStrLn . show . length . filter (==True) . map (contains head) . map (toInt) $ pairs -- part 2
  hClose input
