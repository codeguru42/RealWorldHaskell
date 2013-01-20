import System.Environment (getArgs)

flatten [] = []
flatten (x:xs) = x ++ flatten xs

firstWords s = flatten f
  where l = lines s
        w = map words l
        f = map head (filter (not . null) w)

interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

main = mainWith myFunction
  where mainWith function = do
          args <- getArgs
          case args of
            [input,output] -> interactWith function input output
            _ -> putStrLn "error: exactly two arguments needed"
        -- replace "id" with the name of our function below
        myFunction = firstWords
