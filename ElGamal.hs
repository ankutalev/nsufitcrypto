import Basics
import System.IO

data SystemKey = SystemKey { p::Integer,
                             g::Integer 
                           } deriving Show

type Message = Integer

data SignedMessage = SignedMessage { m::Integer,
                                     r::Integer,
                                     s::Integer
                                    }  deriving Show
data User = User { name::String,
                   c::Integer,
                   d::Integer
                 } deriving (Show)
                        

generateSystemKey::SystemKey
generateSystemKey = SystemKey 31259 2

                        
generateUser::SystemKey->String->IO User
generateUser sysKey name = do 
                 let modulo = p sysKey
                 c <- randomInteger $ modulo - 1
                 print $ "c = " ++ show c
                 let d = fastPow (g sysKey) c modulo
                 print $ "d = " ++ show d
                 return $ User name c d
                            

sendMessage::User-> Message -> Integer -> SystemKey->IO (Integer,Integer)
sendMessage alice message bobPKey sKey = do
                    print $ "Alice crypt " ++ show message
                    let modulo = p sKey
                    r <- randomInteger $ modulo - 2  
                    let k = fastPow (g sKey) r modulo
                    let y = message *  (fastPow  bobPKey  r modulo)
                    return (y,k)

recieveMessage::User->Integer->(Integer,Integer)->Message
recieveMessage bob modulo (y,k) = y * (fastPow  k ( modulo - c bob - 1)   modulo ) 

signMessage::Message->User->SystemKey->SignedMessage
signMessage m (User name c d) sKey = SignedMessage m r s where
                                                              pp = p sKey
                                                              k = last [x| x<-[2..pp-2],gcd x (pp-1) == 1]
                                                              r = fastPow (g sKey) k pp
                                                              u = safeMod (m - c*r) (pp-1)
                                                              s = (inversion k (pp-1) * u) `mod` (pp-1)
                                                                                                 
                                                                     
checkSign::SignedMessage->User->SystemKey->Bool
checkSign (SignedMessage m r s) alice sKey = fastPow (g sKey) m k == (fastPow (d alice) r k * fastPow r s k) `mod` k where
                                                                                                                           k = p sKey                                                          
                                        
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
         print "Let check El Gamal signing!"
         init <- generateUser sKey "trusted Alice"
         let trustedAlice = init {d = 16196, c = 1024 } 
         let signedMessage = SignedMessage { m = 500, r = 27665, s =  26022}
         let x = signMessage message alice sKey
         print x
         print $ checkSign x alice sKey
         print $ checkSign signedMessage trustedAlice sKey
         print $ checkSign signedMessage alice sKey
         print $ checkSign x trustedAlice sKey
         print trustedAlice
     