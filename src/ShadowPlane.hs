module ShadowPlane where

import PlaneRotate

generateShadow :: Double -> Double -> Double -> Double -> Point3D -> Plane -> Plane
generateShadow a b c d light (Plane first second third fourth) 
  										= Plane (evalPoint a b c d light first)
  												(evalPoint a b c d light second)
  												(evalPoint a b c d light third)
  												(evalPoint a b c d light fourth)

generateShadowPoint :: Point3D -> Point3D -> Point3D
generateShadowPoint light point = invert $ scale3D 1.5 $ betweenVertex light point 

betweenVertex :: Point3D -> Point3D -> Point3D
betweenVertex (Point3D x1 y1 z1) (Point3D x2 y2 z2) = (Point3D (x2 - x1) (y2 - y1) (z2 -z1))

invert :: Point3D -> Point3D
invert point = scale3D (-1.0) point 

evalPoint :: Double -> Double -> Double -> Double -> Point3D -> Point3D -> Point3D
evalPoint a b c d (Point3D x0 y0 z0) (Point3D x1 y1 z1) = Point3D (x0 + m * t) (y0 + n * t) (z0 + p * t) where
													m = x1 - x0
													n = y1 - y0
													p = z1 - z0
													t = ( (-a) * x0 - b * y0 - c * z0 - d) / (a * m + b * n + c * p)   



{-
	A(x0, y0, z0) B(x1, y1, z1)
	Plane - ax + by + cz + d = 0

	m = x1 - x0
	n = y1 - y0
	p = z1 - z0


	(x - x0) / m = (y - y0) / n = (z - z0) / p

	{
	| x = x0 + mt
	| y = y0 + nt
	| z = z0 + pt
	| ax + by + cz + d = 0
	{

	a * (x0 + mt) + b * (y0 + nt) + c * (z0 + pt) + d = 0

	ax0 + amt + by0 + bnt + cz0 + cpt + d = 0

	amt + bnt + cpt = -ax0 - by0 - cz0 - d

	t * (am + bn + cp) = -ax0 - by0 - cz0 - d

	t = (-ax0 - by0 - cz0 - d) / (am + bn + cp)
-}
