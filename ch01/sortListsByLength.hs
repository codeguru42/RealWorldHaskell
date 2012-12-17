import Data.List(sortBy)

sortListsByLength :: [[a]] -> [[a]]
sortListsByLength = sortBy (\l1 l2 -> compare (length l1) (length l2))