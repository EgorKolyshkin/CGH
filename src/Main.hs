module Main(main) where

import Graphics.Gloss hiding(Point, scale, translate)
import Bezier
import Engine
import PlaneRotate

resultPlane t = line $ planeToPath (doWithPlaneInOrigin plane (rotatePlane rotate3DX (realToFrac t)))

planeToPath (Plane fst snd thd fth) = [toPair fst, toPair snd, toPair thd, toPair fth]

toPair (Point3D x y z) = (realToFrac x, realToFrac y)

point1 = Point3D 0.1 0.1 0.1
point2 = Point3D 0.3 0.1 0.1
point3 = Point3D 0.1 0.5 0.2
point4 = Point3D 0.3 0.5 0.2

plane = Plane point1 point2 point3 point4

aggregatedPlane :: Plane -> Plane
aggregatedPlane plane = scalePlane 600 plane


window :: Display
window = InWindow "Bezier" (600, 600) (0, 0)

background :: Color
background = white

main :: IO ()
main = animate window background resultPlane


