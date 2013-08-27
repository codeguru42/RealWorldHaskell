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

hcat :: [Doc] -> Doc
hcat xs = undefined

string :: String -> Doc
string = enclose '"' '"' . hcat . map oneChar

enclose :: Char -> Char -> Doc -> Doc
enclose left right x = undefined

oneChar :: Char -> Doc
oneChar c = undefined

text :: String -> Doc
text str = undefined

double :: Double -> Doc
double num = undefined
