module Main(main) where

import Graphics.Gloss
import Bezier
import Engine

width, height, offset :: Int
width = 300
height = 300
offset = 100

path = map toBezierPoint [1..100]
lineBezier = line path

points = [Point 0.0 0.0, Point 0.0 1.0, Point 1.0 1.0, Point 1.0 0.0]
n = 3

bezier = b' n points

toBezierPoint n = let point = bezier (n * 0.01) in ((fromIntegral width) * ((x point) - 0.5), ( fromIntegral height) * ((y point) - 0.5))



window :: Display
window = InWindow "Pong" (600, 600) (offset, offset)

background :: Color
background = white

main :: IO ()
main = display window background lineBezier


