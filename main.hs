calculate :: String -> Int
calculate = head . foldl calcop [] . words

calcop :: [Int] ->  String -> [Int]
calcop (x:y:ys) "+" = (x + y):ys
calcop (x:y:ys) "-" = (x - y):ys
calcop (x:y:ys) "/" = (x `div` y):ys
calcop (x:y:ys) "*" = (x * y):ys
calcop xs c = (read c):xs

main =
  print $ calculate "2 3 + 3 *"
