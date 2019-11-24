module Basics (
	binaryLog,
	toBinary,
	fastPow,
	genericGcd,
	inversion,
	randomPrime,
	randomInteger,
	Modulo,
	Power,
	eulerPrimesFormula,
	safeMod,
	randomRelativeWith
) where

import System.Random

type Power = Integer
type Modulo = Integer

randomInteger::Modulo->IO Integer
randomInteger m = do
				x <- randomIO
				let posx = if x < 0 then (-x) else x
				return $ posx `mod` m
		      

randomRelativeWith::Integer->IO Integer
randomRelativeWith number = do
                              let numbers = [x| x<- [2..number], gcd x number == 1]
                              randomPrime numbers

randomPrime::[Integer]->IO Integer
randomPrime xs = do
				randIndex <-randomIO
				return $ xs !! (randIndex `mod` (length xs))  


binaryLog::Integer->Integer
binaryLog = floor . logBase 2.0 . fromIntegral

toBinary::Integer->[Integer]
toBinary x = helper x [] where
			 helper 0 xs = xs
			 helper x xs = helper ( x `div` 2 ) (x `rem` 2 : xs )

fastPow::Integer->Power->Modulo->Integer
fastPow b p m = helper 1 (powValues b p m) $ toBinary p where
				helper::Integer->[Integer]->[Integer]->Integer
				helper n [] [] = n `rem` m
				helper n (x:xs) (y:ys) | y == 0 = helper n xs ys
									   | otherwise = helper (n*x) xs ys
 
powValues::Integer->Integer->Integer->[Integer]
powValues base pow mod = helper 1 [base] where
										 helper::Integer->[Integer]->[Integer]
										 helper n (x:xs) | n == limit = x:xs
										                 |otherwise = helper (n + 1) (x^2 `rem` mod:x:xs)
										 limit = (binaryLog pow) + 1
										 

genericGcd::Integer->Integer->(Integer, Integer)
genericGcd a b  |b > a = genericGcd b a
			    |otherwise = helper [a,0] [b,1] where 
							 helper::[Integer]->[Integer]->(Integer,Integer)
							 helper [0,_] [gcd,y] = (gcd,y) 
							 helper [gcd,y] [0,_] = (gcd,y)  
							 helper [x1,x2] [y1,y2] = helper [y1,y2] [x1 `mod` y1, x2 - (y2 * (x1 `div` y1 ))]
										 
inversion::Integer->Modulo->Integer
inversion num mod = if d > 0 then d else d + mod where (_,d) = genericGcd num mod		

safeMod::Integer->Modulo->Integer
safeMod num p | num > 0 = num `mod` p
			  | otherwise = safeMod (num + p) p

eulerPrimesFormula::[Integer]->Integer
eulerPrimesFormula = product . map (\x->x-1)   					 

