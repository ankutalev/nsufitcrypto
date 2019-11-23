import Basics
import System.IO

type Message = Integer

data User = User {
				name::String,
				p::Integer,
				q::Integer,
				c::Integer,
				d::Integer,
				n::Integer,
				phi::Integer
			} deriving (Show)
						

generateUser::String->Integer->Integer->User
generateUser name p q =  User name p q c d (p*q) phi where
                                                    phi = eulerPrimesFormula [p,q]
                                                    d   =  [x | x <- [2..phi], gcd x phi == 1] !! 3
                                                    c   = inversion d phi
						

sendMessage::Message->User-> Integer
sendMessage message (User _ _ _ _ d n _) = fastPow message d n

receiveMessage::Message->User->Integer
receiveMessage  message (User _ _ _ c _ n _) = fastPow message c n						
										
main::IO()
main = do 
		 let p = 30803
		 message <- randomInteger p
		 let alice = generateUser "Alice" 131 227
		 let bob = generateUser "Bob" 113 281
		 print alice
		 print bob
		 print $ "Message to sent " ++ show message
		 let e =  sendMessage message bob
		 let got =   receiveMessage e bob
		 if (got==message) then putStrLn "Bob gets message!" else putStrLn "Wrong algo!!"
		 	