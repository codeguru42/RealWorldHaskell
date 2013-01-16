isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome (x:[]) = True
isPalindrome (x:xs) = (last xs) == x && isPalindrome (init xs)
