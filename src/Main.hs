module Main(main) where

import Graphics.Gloss hiding(Point, scale, translate)
import Bezier
import Engine
import PlaneRotate

resultPlane t = 

window :: Display
window = InWindow "Bezier" (600, 600) (offset, offset)

background :: Color
background = white

main :: IO ()
main = animate window background glossResult


