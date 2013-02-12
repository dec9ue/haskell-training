import Data.List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import System.IO
import Control.Monad
import Control.Applicative

factors = factorize default_problem

default_problem = 280671392065546467397265294532969672241810318954163887187279320454220348884327

display s = do
    putStrLn $ s
    hFlush stdout

triple_power [] = [([],[],[])]
triple_power (head:tail) = concat [map (cons1 head) tpt,map(cons2 head) tpt,map(cons3 head) tpt]
    where tpt = triple_power tail

cons1 h (x,y,z) = (h:x,y,z)
cons2 h (x,y,z) = (x,h:y,z)
cons3 h (x,y,z) = (x,y,h:z)

surface_area v = a*b + b*c + c*a
    where (a,b,c) = surface v

surface  (x,y,z) = (product x,product y,product z)

main = do
    display $ "factorized : " ++ show factors
    let m = Map.fromList $ map (\v->(surface_area v,v))$ triple_power factors
    let min_key  = (sort $ Map.keys $ m) !! 0
    display $ "result min : " ++ show (min_key)
    display $ "result min : " ++ show (surface <$> Map.lookup min_key m)
    let min_key2 = (sort $ Map.keys $ m) !! 1
    display $ "result boob: " ++ show (min_key2)
    display $ "result boob: " ++ show (surface <$> Map.lookup min_key2 m)
    let (a,b,c) = fromMaybe (0,0,0) $ (surface <$> Map.lookup min_key m)
    let [x,y,z] = sort $ [a,b,c]
    display $ "final result is : " ++ show x ++ "x" ++ show y ++ "x" ++ show z

factorize n = fromMaybe [n] $ do
       next_factor <- listToMaybe $ filter (\x -> n `mod` x == 0) [2..(intsqrt n)]
       return $ next_factor : factorize (n `div` next_factor)

intsqrt n = binsearch (ordintsqrt n) 1 n

isintsqrt n r = (r+1)*(r+1) > n && r * r <= n
ordintsqrt n r =
    case ((r+1)*(r+1) <= n , r * r <= n) of
    (True ,True)  -> LT
    (False,False) -> GT
    (False,True ) -> EQ
    (_    ,    _) -> undefined

-- ismatch must be a monotonic function over the domain
binsearch ismatch high low =
    case (ismatch high, ismatch low) of
    (EQ, _)  -> high
    ( _,EQ)  -> low
    (LT,GT)  -> binsearch ismatch low high -- fix skew
    _        -> let mid = low+((high-low) `div` 2)  in
        case ismatch mid of
        EQ -> mid
        GT -> binsearch ismatch mid low 
        LT -> binsearch ismatch high mid

