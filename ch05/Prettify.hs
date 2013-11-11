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
fsep = fold (</>)

(</>) :: Doc -> Doc -> Doc
x </> y = x <> softline <> y

softline :: Doc
softline = group line

group :: Doc -> Doc
group x = flatten x `Union` x

flatten :: Doc -> Doc
flatten (x `Concat` y) = flatten x `Concat` flatten y
flatten Line           = Char ' '
flatten (x `Union` _)  = flatten x
flatten other          = other

hcat :: [Doc] -> Doc
hcat = fold (<>)

fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold f = foldr f empty

punctuate :: Doc -> [Doc] -> [Doc]
punctuate p []     = []
punctuate p [d]    = [d]
puncutate p (d:ds) = (d <> p) : punctuate p ds

text :: String -> Doc
text "" = Empty
text s  = Text s

compact :: Doc -> String
compact x = transform [x]
    where transform [] = ""
          transform (d:ds) =
              case d of
                  Empty        -> transform ds
                  Char c       -> c : transform ds
                  Text s       -> s ++ transform ds
                  Line         -> '\n' : transform ds
                  a `Concat` b -> transform (a:b:ds)
                  _ `Union` b  -> transform (b:ds)

pretty = undefined
