import Basics
import System.IO

type X = Integer
type A = Integer
type Y = Integer

getBasicsValues::Modulo->(Integer,Integer)
getBasicsValues p = (m,m) where 
                               m = (floor $ sqrt $ fromIntegral p) + 1

giantStep::A->Y->Modulo->X
giantStep a y p = answer where
                              (m,k) = getBasicsValues p
                              baby = head [ (i, j) | i<-[1..m-1], j<-[1..k], fastPow a (j*m) p == ((fastPow a i p) * y) `mod` p ]
                              answer = snd baby * m - fst baby
main::IO()
main = do
		 let equations = [ (2,24322,30203) ,(2,21740,30323), (2,28620,30539), (2,16190,30803),(5,30994,31607) ]
		 let answers = map (\(x,y,p) -> giantStep x y p) equations
		 print answers
		 putStrLn "Wrong algo!!"
		 	