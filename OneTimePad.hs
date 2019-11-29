import Basics
import System.IO
import System.Random
import Data.Bits
import Data.Maybe
type Message = Integer
type Encrypted = Integer
type Key = Integer


sendMessage::Message->Key->Maybe Encrypted
sendMessage m k | count m > count k  = Nothing
                | otherwise = Just (m `xor` k) where count =  length . show

getMessage::Key->Encrypted->Message
getMessage = xor

generateKey::IO Integer
generateKey = do 
                 c <- randomIO
                 return $ if c > 0 then c else (-c)
                 
main::IO()
main = do
         key <- generateKey
         print $ " Key " ++ show key
         putStrLn "Enter message to send!"
         message <-getLine
         let encoded = sendMessage (read message) key
         if isNothing encoded then do
                                      putStrLn " Length of message must be less or equal key length!"
                               else do      
                                        print $ "Encoded " ++ show encoded
                                        let decoded = getMessage key (fromJust encoded)
                                        print $ "Decoded " ++ show decoded
                                        if  decoded  == read message then putStrLn "Got message!" else putStrLn " Whoops!"   
         main                              