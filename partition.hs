mindiff :: [Int] -> Int
mindiff xs =  let ans = snd $ head $ reverse (filter (\(x,y) -> x) (zip (take (total `div` 2 + 1) (subsum xs !! n)) [0..]))
              in total - ans - ans
  where n = length xs
        total = sum xs

subsum ys = xs
  where xs = map (map (subsum1 ys)) [ map (\y -> (x,y)) [0..] | x <- [0..] ]
        subsum1 ys (x,y)
          | y < 0 = False
          | y == 0 = True
          | x == 0 = False
          | otherwise = let z = y - (ys !! (x - 1))
                        in (xs !! (x - 1) !! y) || (z >= 0 && (xs !! (x - 1) !! z))
