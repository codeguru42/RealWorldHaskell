myLength :: [a] -> Integer
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

mySum :: (Num a) => [a] -> a
mySum [] = 0
mySum (x:xs) = x + mySum xs

mean :: [Double] -> Double
mean xs = mySum xs / (fromIntegral . myLength) xs

main = do
  print (mySum xs)
  print (myLength xs)
  print (mean xs)
  where xs = [1..10]
