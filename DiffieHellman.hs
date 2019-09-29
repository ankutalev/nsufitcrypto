import Basics
import System.IO


data SystemKey = HellmanSystem Integer Modulo deriving Show
type PrivateKey  = Integer

data User = User {
				name::String,
				privateKey::PrivateKey,
				publicKey::Integer
			} deriving (Show)
			



generatePrivateKeys::IO(PrivateKey, PrivateKey)
generatePrivateKeys = withFile "primes.txt" ReadMode 
									  (\h -> do 
											   contents <- hGetContents h 
											   let primesList = (concat.(map(map read)).map words.lines) contents
											   apk <- randomPrime primesList
											   bpk <- randomPrime primesList
											   print $ "Alice private key " ++ (show apk)
											   print $ "Bob  private key " ++ (show bpk)
											   return (apk,bpk)
										)

sendMessage::User->User->Modulo->Integer
sendMessage (User _ aPrivKey aPubKey) (User _ bPrivKey bPubKey) p = fastPow bPubKey aPrivKey p					

generatePublicKey::SystemKey->PrivateKey->Integer
generatePublicKey (HellmanSystem base modulo) pKey = fastPow base pKey modulo

getSystemModulo :: SystemKey->Modulo
getSystemModulo (HellmanSystem _ m ) = m

main::IO()
main = do 
		 (aPrivate,bPrivate) <- generatePrivateKeys
		 let systemKey = HellmanSystem 2 30803 
		 print systemKey
		 let aPublic = generatePublicKey systemKey aPrivate
		 let bPublic = generatePublicKey systemKey bPrivate
		 let alice = User {name= "Alice", privateKey = aPrivate, publicKey = aPublic}
		 let bob = User {name= "Bob", privateKey = bPrivate, publicKey = bPublic}
		 let aliceSecretKey = sendMessage alice bob (getSystemModulo systemKey)
		 let bobSecretKey =   sendMessage bob alice (getSystemModulo systemKey)
		 print alice
		 print bob
		 print $ "Alice Diffie-Hellman secret key " ++ show aliceSecretKey
		 print $ "Bob   Diffie-Hellman secret key " ++  show bobSecretKey
		 
	   
	   