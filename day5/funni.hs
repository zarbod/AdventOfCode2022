move :: Int -> Int -> [String] -> [String]
move x y xs = foldr (\t acc -> if (snd t) == y
  then ((box:(fst t)):acc) else if (snd t) == x then ((tail . fst $ t):acc)
                      else ((fst t):acc)) [] arr
  where arr = zip xs [1..]
        box = head $ xs !! (x - 1)
