import Basics
import System.IO

type Message = Integer

data SignedMessage = SignedMessage { message::Integer,
					                sign ::Integer
					               } deriving (Show)

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

signWithRSA::User->Message->SignedMessage
signWithRSA  user message=  SignedMessage {sign = receiveMessage message user, message = message}

checkValidity::SignedMessage->User->Bool
checkValidity  (SignedMessage message sign) user = receiveMessage message user == sign 
				
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
		 putStrLn " Let's test RSA Sign!"
		 let trustedMessage = SignedMessage 500 46514
		 let initAlice = generateUser "Alice with sign" 227  233 
		 let signedAlice = initAlice {d = 3, c = inversion 3 (phi initAlice)}
		 print signedAlice
		 print trustedMessage
		 print $ checkValidity trustedMessage signedAlice
		 print $ checkValidity  (trustedMessage  {sign = 46515} ) signedAlice
		 print $ checkValidity  (trustedMessage {message = 51232}) signedAlice
		 