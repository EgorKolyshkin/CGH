module Engine where

data Point a = Point a a

class XY c where

	x :: c a -> a

	y :: c a -> a

class XY c => Vertex c where

	translate :: (Num a) => c a -> c a -> c a

	scale :: (Num a) => a -> c a -> c a

	rotate :: (Floating a) => a -> c a -> c a

instance XY Point where
	
	x (Point first second) = first

	y (Point first second) = second

instance Vertex Point where

	translate xy1 xy2 = Point (x xy1 + x xy2) (y xy1 + y xy2)

	scale a xy = Point ((*a) $ x xy) ((*a) $ y xy)

	rotate a xy = Point (x' * cosA - y' * sinA) (x' * sinA + y' * cosA) where
					x' = x xy
					y' = y xy
					cosA = cos a
					sinA = sin a
