module ShadowPlane where

import PlaneRotate

generateShadow :: Double -> Double -> Double -> Point3D -> Plane -> Plane
generateShadow maxX maxY maxZ light (Plane first second third fourth) 
  										= Plane (generateShadowPoint light first)
  												(generateShadowPoint light second)
  												(generateShadowPoint light third)
  												(generateShadowPoint light fourth)

generateShadowPoint :: Point3D -> Point3D -> Point3D
generateShadowPoint light point = invert $ scale3D 1.5 $ betweenVertex light point 

betweenVertex :: Point3D -> Point3D -> Point3D
betweenVertex (Point3D x1 y1 z1) (Point3D x2 y2 z2) = (Point3D (x2 - x1) (y2 - y1) (z2 -z1))

invert :: Point3D -> Point3D
invert point = scale3D (-1.0) point 