splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith pred (x:xs) = (x : takeWhile pred xs) 
                        : splitWith pred (dropWhile pred xs)
