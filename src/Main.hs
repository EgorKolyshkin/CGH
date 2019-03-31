module Main(main) where

import Graphics.Gloss hiding(Point, scale, translate)
import Bezier
import Engine

width, height, offset :: Int
width = 300
height = 300
offset = 0
n = 3

pointsCount = 100.0


path :: Float -> (Float -> Point Float) -> Int -> Path 
path pointsCount bezier t = toPath $ aggregatePoints $ map (\x -> bezier $ (*) (1.0 / pointsCount) x) [0.0..(fromIntegral t)]

lineBezier t = line $ path pointsCount bezierThreeSpline $ mapTime t

points1 = [Point 0.0 0.6, Point 0.5 0.8, Point 0.7 0.8, Point 0.9 0.6]
points2 = [Point 0.9 0.6, Point 1.0 0.4, Point 0.8 0.2, Point 0.5 0.1]
points3 = [Point 0.5 0.1, Point 0.3 0.15, Point 0.2 0.2, Point 0.0 0.6]

b'3 = b' 3

bezier1 = b'3 points1
bezier2 = b'3 points2
bezier3 = b'3 points3

lenghtR = 0.33

bezierThreeSpline t | t <= 0.33 = bezier1 (t / lenghtR)
					| t <= 0.66 = bezier2 ((t - lenghtR) / lenghtR)
					| otherwise = bezier3 ((t- 2 * lenghtR) / lenghtR)


mapTime :: Float -> Int
mapTime t = mod (floor (t * 20)) 100

offsetCoord = -0.5

glossResult t = pictures [color red (lineBezier t), color blue (line $ toPath $ aggregatePoints (points1 ++ points2 ++ points3))]

aggregatePoints :: [Point Float] -> [Point Float]
aggregatePoints points = scalePoints (fromIntegral width) $ translatePoints (Point offsetCoord offsetCoord) points

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


