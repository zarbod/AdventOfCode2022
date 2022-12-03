import System.IO
import Data.List.Split
import Data.Char (ord)

getVal :: Char -> Int
getVal a
  | a <= 'Z' && a >= 'A' = (ord a) - 38
  | otherwise = (ord a) - 96

common :: (String, String) -> Int
common (a, b) = getVal . head . filter (\x -> x `elem` b) $ a

commonSum :: [(String, String)] -> Int
commonSum x = sum . map (common) $ x

common3 :: String -> String -> String -> Int
common3 x y z = getVal . head . filter (\a -> a `elem` y && a `elem` z) $ x

threeSum :: [String] -> Int
threeSum [] = 0
threeSum (x:y:z:xs) = (common3 x y z) + threeSum xs

main :: IO ()
main = do
  input <- openFile "input.txt" ReadMode
  contents <- hGetContents input
  let rucksacks = filter (/= "") . splitOn "\n" $ contents
  putStrLn . show $ commonSum $ map (\x -> splitAt (length x `div` 2) x) rucksacks -- part 1
  putStrLn . show . threeSum $ rucksacks -- part 2
  hClose input
