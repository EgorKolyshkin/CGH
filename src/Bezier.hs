module Bezier where

import Engine
{-
	Bezier curves
	n - polynom grade	
	P - (n + 1) points
	B(t) = sum from 0 to n with k 
			Pk * bkn(t) where
				0 <= t <= 1
				bkn(t) = Cnk * (t ^ k) * (1 - t) ^ (n - k)
-}

b' :: Int -> [Point Float] -> Float -> Point Float
b' n points t | length points /= n + 1 = error "IllEgal points list"
			  | otherwise = ifoldr (accumulate t n) (Point 0.0 0.0) points

accumulate :: Float -> Int -> Int -> Point Float -> Point Float -> Point Float
accumulate t n k new acc= (translate acc) $ (scale (b n k t) new) 

b :: Int -> Int -> Float -> Float
b n k t = (fromIntegral (cnk n k)) * (t ^ k) * ((1.0 - t) ^ (n - k))

factorial :: Int -> Int
factorial n = foldl (*) 1 [1..n]

cnk :: (Int) -> (Int) -> (Int)
cnk n k = div (factorial n) ((factorial k) * (factorial (n - k)))

ifoldr :: (Int -> a -> b -> b) -> b -> [a] -> b
ifoldr f z xs = foldr (\x g i -> f i x (g (i+1))) (const z) xs 0

