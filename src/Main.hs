module Main(main) where

import Graphics.Gloss hiding(Point, scale)
import Bezier
import PlaneRotate
import ShadowPlane

offsetX = -0.5
offsetY = -0.5


resultPlane t = Pictures [color (chooseColor newPlane) $ polygon $ planeToPath $ aggregatedPlane $ newPlane, line $ planeToPath $ aggregatedPlane newPlane,
		 polygon $ planeToPath $ aggregatedPlane $ shadowPlane,
		 planeLightLines (aggregatedPoint lightPoint) (aggregatedPlane shadowPlane) ] where
		 	newPlane = (doWithPlaneInOrigin plane (transformPlane (t / 5)))
		 	shadowPlane = generateShadow 0.0 1.0 0.0 0.0 lightPoint newPlane
		 	lightPoint = generateLightPoint t (Point3D 0.5 2.0 0.5)

generateLightPoint :: Float -> Point3D -> Point3D
generateLightPoint t (Point3D fst snd thd) = (Point3D (integratedT t) snd thd)

integratedT :: Float -> Double 
integratedT t = (realToFrac $ ((sin newT) + 1.0)) / 2.0 where
				newT = t / 10.0   

transformPlane :: Float -> Plane -> Plane
transformPlane t plane = rotatePlane rotate3DY (realToFrac (1.5 * t)) $ rotatePlane rotate3DX (realToFrac (t)) plane

planeToPath (Plane fst snd thd fth) = [toPair fst, toPair snd, toPair thd, toPair fth, toPair fst]

shadowToPath (Plane fst snd thd fth) = [toPairShadow fst, toPairShadow snd, toPairShadow thd, toPairShadow fth, toPairShadow fst]

toPair (Point3D x y z) = (realToFrac (x / optimalZ) , realToFrac (y / optimalZ)) where
							optimalZ = (z / 300) + 1

toPairShadow (Point3D x y z) = (realToFrac x, realToFrac y)

point1 = Point3D 0.3 0.3 0.1
point2 = Point3D 0.3 0.7 0.1
point3 = Point3D 0.5 0.7 0.1
point4 = Point3D 0.5 0.3 0.1

plane = Plane point1 point2 point3 point4


planeLightLines :: Point3D -> Plane -> Picture 
planeLightLines light (Plane first second third fourth) = let lightPair = toPair light in 
						Pictures [line [(toPair first), lightPair], 
								line [(toPair second), lightPair],
								line [(toPair third), lightPair],
								line [(toPair fourth), lightPair]]


chooseColor :: Plane -> Color
chooseColor plane | (colorX + colorY) == 1 = red
			      | otherwise = green where
			      	colorY = chooseColorForY plane
			      	colorX = chooseColorForX plane


chooseColorForY :: Plane -> Int
chooseColorForY (Plane first second third fourth) | (firstX + secondX - thirdX - fourthX) > 0 = 0
												  | otherwise = 1 where
													firstX = pointX first
													secondX = pointX second
													thirdX = pointX third
													fourthX = pointX fourth 

chooseColorForX :: Plane -> Int
chooseColorForX (Plane first second third fourth) | (secondY + thirdY - firstY - fourthY) > 0 = 0
												  | otherwise = 1 where
													firstY = pointY first
													secondY = pointY second
													thirdY = pointY third
													fourthY = pointY fourth 

chooseColorFor :: Float -> Color
chooseColorFor t | fromIntegral (mod (floor ((t + pi / 2) / pi )) 2) == 0 = red
				 | otherwise = green

aggregatedPlane :: Plane -> Plane
aggregatedPlane plane = scalePlane 600 $ translatePlane plane (Point3D offsetX offsetY 0.0) 

aggregatedPoint :: Point3D -> Point3D
aggregatedPoint point = scale3D 600 $ translate3D point (Point3D offsetX offsetY 0.0) 


window :: Display
window = InWindow "Bezier" (600, 600) (0, 0)

background :: Color
background = white

main :: IO ()
main = animate window background resultPlane


