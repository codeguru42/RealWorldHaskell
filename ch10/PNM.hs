import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import Data.Char (isSpace)

data Greymap = Greymap {
      greyWidth :: Int
    , greyHeight :: Int
    , greyMax :: Int
    , greyData :: L.ByteString
} deriving (Eq)

instance Show Greymap where
    show (Greymap w h m _) = "Greymap " ++ show w ++ "x" ++ show h ++ " " ++ show m

matchHeader :: L.ByteString -> L.ByteString -> Maybe L.ByteString
mathcHeader = undefined

getNat :: L.ByteString -> Maybe (Int, L.ByteString)
getNat = undefined

getBytes :: Int -> L.ByteString -> Maybe (L.ByteString, L.ByteString)
getBytes = undefined

parseP5 :: L.ByteString -> Maybe (Greymap, L.ByteString)
parseP5 = undefined
