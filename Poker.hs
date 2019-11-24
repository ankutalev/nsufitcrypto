import Basics
import System.IO
import System.Random.Shuffle
import Control.Monad.Random

data Cards = Joker | King | Queen deriving Show
data CardsValues = CardsValues Cards Integer deriving Show

data User  = User { name::String,
                    c::Integer,
                    d::Integer
                  } deriving Show

generateUser::String->OpenPrime->IO User
generateUser name p = do
                        c <- randomRelativeWith (p-1)
                        let d = inversion c (p-1)
                        return $ User name c d
                  
type OpenPrime = Integer        
getOpenPrime::IO OpenPrime
getOpenPrime = withFile "primes.txt" ReadMode (\h -> do 
                                                       contents <- hGetContents h 
                                                       let primesList = (concat.(map(map read)).map words.lines) contents
                                                       c <- randomPrime primesList
                                                       print c
                                                       return c
                                               )          
generateDeck::OpenPrime->IO [CardsValues]
generateDeck p = do
                   jValue <- randomRelativeWith (p-1)
                   kValue <- randomRelativeWith (p-1)
                   qValue <- randomRelativeWith (p-1)
                   return [CardsValues Joker jValue, CardsValues King kValue, CardsValues Queen qValue]
                   
powShuffle:: MonadRandom m => [Integer]->User->OpenPrime->m [Integer]
powShuffle deck (User _ c _) p = shuffleM  $  map (\x->fastPow x c p) deck

getAliceCard::[CardsValues]->[Integer]->User->User->OpenPrime->IO (CardsValues,[Integer])
getAliceCard deck shuffled alice bob p  = do
                                       bobPick <- randomPrime shuffled
                                       let encrypted = fastPow bobPick (d alice) p
                                       return (head $ filter (\(CardsValues _ v) -> v == encrypted) deck, filter (\x->x/=bobPick) shuffled )

getBobCard::[CardsValues]->[Integer]->User->User->OpenPrime->IO CardsValues
getBobCard deck bobShuffle alice bob p = do
                                            alicePick <- randomPrime bobShuffle
                                            let encrypted = fastPow alicePick (d alice) p
                                            let decrypted = fastPow encrypted (d bob) p
                                            return $ head $ filter (\(CardsValues _ v) -> v == decrypted) deck
 
main = do
         p <- getOpenPrime
         alice <- generateUser "Alice" p
         bob <- generateUser "Bob" p
         deck <-generateDeck p
         print alice
         print bob
         print deck
         let cardValues = map (\(CardsValues _ x)-> x) deck
         shuffled <- powShuffle cardValues  alice p
         print shuffled
         (aliceCard, others) <- getAliceCard deck shuffled alice bob p
         print $ "Alice got " ++  show aliceCard
         bobShuffled <- powShuffle others bob p
         bobCard <- getBobCard deck bobShuffled alice bob p
         print $ "Bob got " ++  show bobCard