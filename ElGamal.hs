import Basics
import System.IO

data SystemKey = SystemKey { p::Integer,
		             g::Integer 
                           } deriving Show

type Message = Integer

data User = User { name::String,
                   c::Integer,
                   d::Integer
                 } deriving (Show)
						

generateSystemKey::SystemKey
generateSystemKey = SystemKey 30803 2
						
generateUser::SystemKey->String->IO User
generateUser sysKey name = do 
			     let modulo = p sysKey
			     c <- randomInteger $ modulo - 1
			     print $ "c = " ++ show c
                             let d = fastPow (g sysKey) c modulo
			     print $ "d = " ++ show d
			     return $ User name c d
							

sendMessage::User-> Integer -> Integer -> SystemKey->IO (Integer,Integer)
sendMessage alice message bobPKey sKey = do
				    print $ "Alice crypt " ++ show message
			            let modulo = p sKey
				    r <- randomInteger $ modulo - 2  
				    let k = fastPow (g sKey) r modulo
				    let y = message *  (fastPow  bobPKey  r modulo)
				    return (y,k)

recieveMessage::User->Integer->(Integer,Integer)->Integer
recieveMessage bob modulo (y,k) = y * (fastPow  k ( modulo - c bob - 1)   modulo ) 
										
main::IO()
main = do  
	 let sKey = generateSystemKey
	 alice <- generateUser  sKey "Alice"
	 bob <- generateUser sKey "Bob" 
	 let modulo = p sKey
	 message <- randomInteger $ modulo - 1	
	 print alice
	 print bob
	 (y,k) <- sendMessage alice message (d bob) sKey
	 let recieved = recieveMessage bob modulo  (y,k) `mod` modulo
	 print recieved
	 print message
	 if (recieved==message) then putStrLn "Bob gets message!" else putStrLn "Wrong algo!!"	
