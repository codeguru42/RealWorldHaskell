module PrettyJSON where

import SimpleJSON

import Data.List (intercalate)

renderJValue :: JValue -> Doc
renderJValue (JBool True)  = text "true"
renderJValue (JBool False) = text "false"
renderJValue (JNull)       = text "null"
renderJValue (JNumber num) = double num
renderJValue (JString str) = string str

data Doc = ToBeDefined
    deriving (Show)

(<>) :: Doc -> Doc -> Doc
a <> b = undefined

hcat :: [Doc] -> Doc
hcat xs = undefined

string :: String -> Doc
string = enclose '"' '"' . hcat . map oneChar

enclose :: Char -> Char -> Doc -> Doc
enclose left right x = char left <> x <> char right

oneChar :: Char -> Doc
oneChar c = case lookup c simpleEscapes of
    Just r -> text r
    Nothing | mustEscape c -> hexEscape c
            | otherwise    -> char c
    where mustEscape c = c < ' ' || c == '\x7f' || c > '\xff'

char :: Char -> Doc
char c = undefined

simpleEscapes :: [(Char, String)]
simpleEscapes = zipWith ch "\b\n\f\r\t\\\"/" "bnfrt\\\"/"
    where ch a b = (a, ['\\', b])

hexEscape :: Char -> Doc
hexEscape = undefined

text :: String -> Doc
text str = undefined

double :: Double -> Doc
double num = undefined
