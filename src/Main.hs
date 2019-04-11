module Main(main) where

import Graphics.Gloss hiding(Point, scale, translate)
import Bezier
import Engine
import PlaneRotate

offsetX = -0.5
offsetY = -0.5

resultPlane t = line $ planeToPath (aggregatedPlane (doWithPlaneInOrigin plane (transformPlane t)))

transformPlane :: Float -> Plane -> Plane
transformPlane t plane = rotatePlane rotate3DY (realToFrac (t / 2.0)) $ rotatePlane rotate3DX (realToFrac t) plane

planeToPath (Plane fst snd thd fth) = [toPair fst, toPair snd, toPair thd, toPair fth, toPair fst]

toPair (Point3D x y z) = (realToFrac x, realToFrac y)

point1 = Point3D 0.3 0.3 0.1
point4 = Point3D 0.5 0.3 0.1
point2 = Point3D 0.3 0.7 0.2
point3 = Point3D 0.5 0.7 0.2

plane = Plane point1 point2 point3 point4

aggregatedPlane :: Plane -> Plane
aggregatedPlane plane = scalePlane 600 $ translatePlane plane (Point3D offsetX offsetY 0.0) 


window :: Display
window = InWindow "Bezier" (600, 600) (0, 0)

background :: Color
background = white

main :: IO ()
main = animate window background resultPlane


