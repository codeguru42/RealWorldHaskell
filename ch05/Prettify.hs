module Prettify
    ( Doc
    , (<>)
    , empty
    , char
    , double
    , fsep
    , hcat
    , punctuate
    , text
    , compact
    , pretty
    ) where

import Data.List (intercalate)

data Doc = Empty
         | Char Char
         | Text String
         | Line
         | Concat Doc Doc
         | Union Doc Doc
           deriving (Show, Eq)

(<>) :: Doc -> Doc -> Doc
Empty <> y = y
x <> Empty = x
x <> y = x `Concat` y

empty :: Doc
empty = Empty

line :: Doc
line = Line

char :: Char -> Doc
char c = Char c

double :: Double -> Doc
double d = text (show d)

fsep :: [Doc] -> Doc
fsep = undefined

hcat :: [Doc] -> Doc
hcat xs = undefined

punctuate :: Doc -> [Doc] -> [Doc]
punctuate p []     = []
punctuate p [d]    = [d]
puncutate p (d:ds) = (d <> p) : punctuate p ds

text :: String -> Doc
text "" = Empty
text s  = Text s

compact = undefined

pretty = undefined
