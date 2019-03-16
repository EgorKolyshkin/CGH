module Main(main) where

import Graphics.Gloss hiding(Point, scale, translate)
import Bezier
import Engine

width, height, offset :: Int
width = 300
height = 300
offset = 0

pointsCount = 100.0
path t = toPath $ aggregatePoints $ map (\x -> bezier $ (*) (1.0 / pointsCount) x) [0.0..(fromIntegral t)]
lineBezier t = line $ path $ mapTime t

mapTime :: Float -> Int
mapTime t = mod (floor (t * 20)) 100

points = [Point 0.0 0.0, Point 0.25 1.0, Point 0.75 1.0, Point 1.0 0.0, Point 0.5 0.0, Point 0.5 0.5]
n = 5
offsetCoord = -0.5

glossResult t = pictures [lineBezier t, line $ toPath $ aggregatePoints points]

aggregatePoints :: [Point Float] -> [Point Float]
aggregatePoints points = scalePoints (fromIntegral width) $ translatePoints (Point offsetCoord offsetCoord) points

bezier :: Float -> Point Float
bezier = b' n points

translatePoints :: Point Float -> [Point Float] -> [Point Float]
translatePoints point points = map (translate point) points

scalePoints :: Float -> [Point Float] -> [Point Float]
scalePoints s points = map (scale s) points

toPath :: [Point Float] -> Path
toPath points = map (\(Point x y) -> (x, y)) points

window :: Display
window = InWindow "Bezier" (600, 600) (offset, offset)

background :: Color
background = white

main :: IO ()
main = animate window background glossResult


