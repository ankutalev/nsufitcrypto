import Basics
import System.IO

type SystemKey = Integer
type Message = Integer

data User = User {
				name::String,
				c::Integer,
				d::Integer
			} deriving (Show)
						

						
generatePrivateKeys::SystemKey->IO(Integer,Integer)
generatePrivateKeys p = withFile "primes.txt" ReadMode (\h -> do 
								contents <- hGetContents h 
						                let primesList = (concat.(map(map read)).map words.lines) contents
								c <- randomPrime primesList
							        let d = inversion (c `mod` (p-1)) (p-1)
								print $ "c " ++ (show c)
								print $ "d " ++ (show d)
								return (c,d)
							)

sendMessage::String->String->Integer->Power->Modulo->IO Integer
sendMessage name receiver i p m = do
					print $ name ++ " crypt "++ show i 
					let x = fastPow i p m
					print $ receiver ++ " get "++ show x
					return x
										
main::IO()
main = do 
		  let p = 30803
		  message <- randomInteger p
		  (ca,da) <- generatePrivateKeys p
		  (cb,db) <- generatePrivateKeys p
		  let alice = User{ name = "Alice", c = ca, d = da}
		  let bob = User{ name = "Bob", c = cb, d = db}
		  let aliceName = name alice
		  let bobName = name bob	
		  print alice
		  print bob
		  print $ "Message to sent " ++ show message
		  x1 <- sendMessage aliceName bobName message ca p
		  x2 <- sendMessage bobName   aliceName x1 cb p
		  x3 <- sendMessage aliceName bobName x2 da p
		  x4 <- sendMessage bobName bobName x3 db p
		  if (x4==message) then putStrLn "Bob gets message!" else putStrLn "Wrong algo!!"
		 	
