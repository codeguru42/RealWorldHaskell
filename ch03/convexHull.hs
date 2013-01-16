import Data.List(minimumBy, sortBy, permutations)

data Direction = LeftTurn
               | RightTurn
               | Straight
               deriving (Show, Eq)

opposite :: Direction -> Direction
opposite LeftTurn = RightTurn
opposite RightTurn = LeftTurn
opposite Straight = Straight

type Point = (Double, Double)

type VectorRect = (Double, Double)

type VectorPolar = (Double, Double)

diffPoints :: Point -> Point -> VectorRect
diffPoints (x2, y2) (x1, y1) = (x2 - x1, y2 - y1)

addVectors :: VectorRect -> VectorRect -> VectorRect
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

toPolar :: VectorRect -> VectorPolar
toPolar (x, y) = (sqrt (x^2 + y^2), atan2 y x)

toRect :: VectorPolar -> VectorRect
toRect (r, theta) = (r * cos theta, r * sin theta)

dot :: VectorRect -> VectorRect -> Double
dot (x1, y1) (x2, y2) = x1 * x2 + y1 * y2

r :: Point -> Double
r p = r'
  where (r', _) = toPolar p

theta :: Point -> Double
theta p = theta'
  where (_, theta') = toPolar p

turn :: Point -> Point -> Point -> Direction
turn (x1, y1) (x2, y2) (x3, y3)
  | cross > 0 = LeftTurn
  | cross < 0 = RightTurn
  | otherwise = Straight
  where cross = (x2 - x1) * (y3 - y1) - (y2 - y1) * (x3 - x1)

allTurns :: [Point] -> [Direction]
allTurns ps = Straight : allTurns' ps
  where allTurns' (a:b:c:[]) = turn a b c : []
        allTurns' (a:b:c:ps) = turn a b c : allTurns' (b:c:ps)

convexHull :: [Point] -> [Point]
convexHull ps = convexHull' sortedPs (allTurns sortedPs)
  where p = minimumBy 
            (\(x1, y1) (x2, y2) -> 
              if compare y1 y2 == EQ 
              then compare x1 x2 
              else compare y1 y2) 
            ps
        minTheta = minimum (map theta ps)
        maxTheta = maximum (map theta ps)
        sortedPs = sortBy
                   (\p1 p2 -> 
                     if theta p1 == minTheta && theta p2 == minTheta
                     then compare (r p1) (r p2)
                     else if theta p1 == maxTheta && theta p2 == maxTheta
                          then compare (r p2) (r p1)
                          else compare (theta p1) (theta p2))
                   ps
        convexHull' (a:b:[]) _ = a:b:[]
        convexHull' as ts
          | RightTurn `elem` ts = convexHull' as' (allTurns as')
          | otherwise = as
            where as' = removeFirstRightTurn as ts
                  removeFirstRightTurn [] [] = []
                  removeFirstRightTurn (a:as) (RightTurn:ts) = as
                  removeFirstRightTurn (a:as) (_:ts) = 
                    a : removeFirstRightTurn as ts

main = do
  print ("ps: " ++ show ps)
  print ("convexHull ps: " ++ show (convexHull ps))
  print ("a permutation of ps: " ++ show (allPs !! 121))
  print ("convexHull of the permutation: " ++ show (convexHull (allPs !! 121)))
  print (all (== convexHull (allPs !! 0)) (map convexHull allPs))
  where ps = [(x, y) | x <- [0..4], y <- [0..4]]
        allPs = permutations ps
