import System.IO
import qualified Data.Set as Set

marker' :: String -> Int -> Int
marker' "" _ = 0
marker' xs n = if Set.size set == n then 1 else 1 + marker' (drop 1 $ xs) n
  where set = Set.fromList (take n $ xs)

main :: IO ()
main = do
  input <- openFile "input.txt" ReadMode
  contents <- hGetContents input
  print $ ((+) 3 $ marker' contents 4) -- part 1
  print $ ((+) 13 $ marker' contents 14) -- part 2
  hClose input
