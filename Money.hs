import qualified Crypto.Hash.MD5 as MD5
import Data.ByteString.Base16 (encode)
import Data.ByteString.Char8 (pack, unpack)
import System.Random (randomIO)
import Basics
import System.IO
import Data.Char
hash :: Integer -> Integer
hash = foldl (+) 0  . map  (toInteger . ord ) . unpack . encode . MD5.hash . pack . show

data Bank = Bank {
                    p::Integer,
                    q::Integer,
                    c::Integer,
                    d::Integer,
                    nn::Integer
                 } deriving Show
data Money = Money { value::Integer, fn::Integer} deriving Show
                 
type OpenPrime = Integer        
getOpenPrime::IO OpenPrime
getOpenPrime = withFile "primes.txt" ReadMode (\h -> do 
                                                       contents <- hGetContents h 
                                                       let primesList = (concat.(map(map read)).map words.lines) contents
                                                       c <- randomPrime primesList
                                                       print c
                                                       return c
                                               )                    
                 
                 
              
generateBank::IO Bank
generateBank = do 
                 p <- getOpenPrime
                 q <- getOpenPrime
                 let phi = eulerPrimesFormula [p,q]
                 let d   =  [x | x <- [2..phi], gcd x phi == 1] !! 6
                 let c   = inversion d phi
                 return $ Bank p q c d (p*q)
                                                 

getN:: IO Integer
getN = fmap (\x-> if x< 0 then (-x) else x) randomIO

getMoney::Integer->Bank->Money
getMoney n bank = Money n signed where
                                       signed = fastPow (hash n) (c bank) (nn bank)

checkValidity::Money->Bank->Bool
checkValidity (Money n fn) bank = fastPow fn (d bank) (nn bank) == hash n

main = do
         n <- getN
         bank <- generateBank
         let money = getMoney n bank
         print money
         print bank
         print $ checkValidity (money { value = value money + 45}) bank
         print $ checkValidity (money { fn = fn money - 1}) bank
         print $ checkValidity (money { value = value money - 445, fn = fn money + 122}) bank
         print $ checkValidity money bank
         otherBank <-generateBank
         putStrLn "Check new Bank "
         print otherBank
         print $ checkValidity (money { value = value money + 45}) otherBank
         print $ checkValidity (money { fn = fn money - 1}) otherBank
         print $ checkValidity (money { value = value money - 445, fn = fn money + 122}) otherBank
         print $ checkValidity money otherBank
         
         