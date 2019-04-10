module PlaneRotate where

data Point3D = Point3D Double Double Double deriving (Show)

data Plane = Plane Point3D Point3D Point3D Point3D deriving (Show)

translate3D :: Point3D -> Point3D -> Point3D
translate3D (Point3D x1 y1 z1) (Point3D x2 y2 z2) = Point3D (x1 + x2) (y1 + y2) (z1 + z2)

scale3D :: Double -> Point3D -> Point3D
scale3D scale (Point3D x y z) = Point3D (scale * x) (scale * y) (scale * z)


rotate3D :: Double -> (Double -> Double -> Point3D) -> Point3D
rotate3D alpha rotate = rotate sinA cosA where 
								  sinA = sin alpha
								  cosA = cos alpha

rotate3DX :: Double -> Point3D -> Point3D
rotate3DX alpha (Point3D x y z) 
			= rotate3D alpha (\sinA cosA -> Point3D (x) (y * cosA - z * sinA) (y * sinA + z * cosA)) 

rotate3DY :: Double -> Point3D -> Point3D
rotate3DY alpha (Point3D x y z) 
			= rotate3D alpha (\sinA cosA -> Point3D (x * cosA + z * sinA) (y) (z * cosA - x * sinA))


rotate3DZ :: Double -> Point3D -> Point3D
rotate3DZ alpha (Point3D x y z) 
			= rotate3D alpha (\sinA cosA -> Point3D (x * cosA - y * sinA) (x * sinA + y * cosA) (z))  


translatePlane :: Plane -> Point3D -> Plane
translatePlane (Plane fst snd thd fth) point
 	= Plane (translate3D fst point) (translate3D snd point) (translate3D thd point) (translate3D fth point)


selectByOption :: (a -> a -> a) -> (Point3D -> a) -> Plane -> a
selectByOption comparator selector (Plane fst snd thd fth)
	 = comparator (comparator fstSelect sndSelect) (comparator thdSelect fthSelect) where
	 			fstSelect = selector fst
	 			sndSelect = selector snd
	 			thdSelect = selector thd
	 			fthSelect = selector fth

selectMin :: (Point3D -> Double) -> Plane -> Double
selectMin = selectByOption (\fst snd -> if fst <= snd then fst else snd)

selectMax :: (Point3D -> Double) -> Plane -> Double
selectMax = selectByOption (\fst snd -> if fst >= snd then fst else snd)

pointX :: Point3D -> Double
pointX (Point3D x _ _) = x

pointY :: Point3D -> Double
pointY (Point3D _ y _) = y

pointZ :: Point3D -> Double
pointZ (Point3D _ _ z) = z

planeHalf :: (Point3D -> Double) -> Plane -> Double
planeHalf selector plane = abs ((selectMax selector plane) - (selectMin selector plane)) / 2.0

translateToOriginPoint :: Plane -> Point3D
translateToOriginPoint plane = Point3D translateX translateY translateZ where
							translateX = (planeHalf pointX plane) - (selectMax pointX plane) 
							translateY = (planeHalf pointY plane) - (selectMax pointY plane) 
							translateZ = (planeHalf pointZ plane) - (selectMax pointZ plane) 

doWithPlaneInOrigin :: Plane -> (Plane -> Plane) -> Plane
doWithPlaneInOrigin plane action
	 = translatePlane (action (translatePlane plane translatePoint)) negativeTranslatePoint where
	 				translatePoint = translateToOriginPoint plane
	 				negativeTranslatePoint = scale3D (-1.0) translatePoint



