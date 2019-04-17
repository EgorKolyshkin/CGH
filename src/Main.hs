module Main(main) where

import Graphics.Gloss hiding(Point, scale, translate)
import Bezier
import Engine
import PlaneRotate
import CutLines

offsetX = -0.5
offsetY = -0.5

width = 600
height = 600
scaleWidth = 300
scaleHeight = 300

first (a, b) = a
second (a, b) = b 

pointLeftUp = Point3D 0.0 1.0 0.00001
pointLeftDown = Point3D 0.0 1.0 1.0

pointLeftBotUp = Point3D 0.0 0.0 0.00001
pointLeftBotDown = Point3D 0.0 0.0 1.0

pointRightUp = Point3D 1.0 1.0 0.00001
pointRightDown = Point3D 1.0 1.0 1.0

pointRightBotUp = Point3D 1.0 0.0 0.00001
pointRightBotDown = Point3D 1.0 0.0 1.0

leftUpLine = Line3D pointLeftUp pointLeftDown
leftDownLine = Line3D pointLeftBotUp pointLeftBotDown
rightUpLine = Line3D pointRightUp pointRightDown
rightDownLine = Line3D pointRightBotUp pointRightBotDown

perspectiveLines = [leftUpLine, leftDownLine, rightUpLine, rightDownLine]

actualResult t = result startPlane linы t

downRectPath t = [
	toPair t $ aggregatedPoint pointLeftBotDown,
	toPair t $ aggregatedPoint pointLeftDown,
	toPair t $ aggregatedPoint pointRightDown,
	toPair t $ aggregatedPoint pointRightBotDown,
	toPair t $ aggregatedPoint pointLeftBotDown]

animatedResult t = 
		let newT = t * 10.0 in
	-- Pictures [line $ planeToPath $ aggregatedPlane $ first $ actualResult t]
		Pictures 
		([line $ planeToPath newT $ aggregatedPlane $ first $ actualResult newT]
			++ (map (color $ greyN 0.5) $ map (toLine newT) $ map aggregatedLine3D perspectiveLines)
			++ map (color $ greyN 0.5) [line $ downRectPath t]
			++ 	(map (markedLineToPicture newT) $ map aggregatedLine $ second $ actualResult newT))

planeToPath t (Plane fst snd thd fth) = [toPair t fst, toPair t snd, toPair t thd, toPair t fth, toPair t fst]

aggregatedPlane :: Plane -> Plane
aggregatedPlane plane = scalePlane scaleWidth $ translatePlane plane (Point3D offsetX offsetY 0.0)

aggregatedLine :: MarkedLine -> MarkedLine
aggregatedLine (MarkedLine line marked) = MarkedLine (aggregatedLine3D line) marked 

aggregatedLine3D :: Line3D -> Line3D
aggregatedLine3D (Line3D first second) = Line3D (aggregatedPoint first) (aggregatedPoint second)

aggregatedPoint :: Point3D -> Point3D
aggregatedPoint point = scale3D scaleWidth $ translate3D point (Point3D offsetX offsetY 0.0)

toPair :: Float -> Point3D -> (Float, Float)
toPair t point = (realToFrac $ (x / z), realToFrac $ (y / z)) where
						x = pointX point
						y = pointY point
						z = (pointZ point) / 100
						offsetX = scaleWidth / 2
						offsetY = 0.0


markedLineToPicture :: Float -> MarkedLine -> Picture
markedLineToPicture t (MarkedLine line Marked) = color red $ toLine t line
markedLineToPicture t (MarkedLine line Unmarked) =  color green $ toLine t line

toLine :: Float -> Line3D -> Picture
toLine t (Line3D first second) = line [(toPair t first), (toPair t second)]


window :: Display
window = InWindow "Bezier" (width, height) (0, 0)

background :: Color
background = white

main :: IO ()
main = animate window background animatedResult
