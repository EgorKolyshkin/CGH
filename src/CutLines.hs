module CutLines where

import PlaneRotate

data Line3D = Line3D Point3D Point3D deriving (Show)
data Marked = Marked | Unmarked deriving (Show)
data MarkedLine = MarkedLine Line3D Marked deriving (Show)

point1 = Point3D 0.3 0.3 0.1
point2 = Point3D 0.3 0.7 0.2

point3 = Point3D 0.5 0.7 0.4
point4 = Point3D 0.5 0.3 0.3

line1 = Line3D point1 point2
line2 = Line3D point3 point4

linы = map lineFromInt [1 .. 10]

topStart = Point3D 0.0 1.0 0.0
bottomStart = Point3D 0.0 0.0 0.0
topEnd = Point3D 1.0 1.0 0.0
bottomEnd = Point3D 1.0 0.0 0.0

startPlane = Plane bottomStart topStart topEnd bottomEnd

lineFromInt :: Integer -> Line3D
lineFromInt i = Line3D (pointFromInt (i * 12)) (pointFromInt (i * 13))

pointFromInt :: Integer -> Point3D
pointFromInt i = Point3D (rand (i * 5)) (rand (i * 6)) (rand (i * 7))

start :: Line3D -> Point3D
start (Line3D first _) = first

end :: Line3D -> Point3D
end (Line3D _ second) = second

maxZ :: Plane -> Double
maxZ = selectMax pointZ 

markLine :: Plane -> Line3D -> MarkedLine
markLine plane line = MarkedLine line $ getMarked (pointZ $ end line) (pointZ $ start line) (maxZ plane)

getMarked :: Double -> Double -> Double -> Marked
getMarked start end value | min start end <= value = Marked
						| otherwise = Unmarked

animatePlane :: Float -> Plane -> Plane
animatePlane t plane = translatePlane plane (Point3D 0.0 0.0 (actualT t))

result :: Plane -> [Line3D] -> Float -> (Plane, [MarkedLine])
result plane lines t = let newPlane = animatePlane t plane in 
					(newPlane, map (markLine newPlane) lines)


actualT :: Float -> Double
actualT t = (realToFrac (mod (floor t) 100)) / 100 

rand :: Integer -> Double
rand k = let next = k * 11035112542535245 in
			(fromInteger (mod (next) 100)) / 100.0




