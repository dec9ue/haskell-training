import Data.Map (Map)
import qualified Data.Map as Map
import System.IO
import Control.Monad

factors = [162425297,
           215940091,
           358456949,
           369941863,
           369941863,
           479871607,
           706170617,
           481362815814826159]

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

surface_area (x,y,z) = a*b + b*c + c*a
    where (a,b,c) = (product x,product y,product z)

main = do
    let m = Map.fromList $ map (\v->(surface_area v,v))$ triple_power factors
    let min_key = minimum $ Map.keys $ m
    display $ "result : " ++ show (min_key)
    display $ "result : " ++ show (Map.lookup min_key m)


intsqrt n = binsearch (ordintsqrt n) 1 n
intproot p n = binsearch (ordintproot p n) 1 n

isintsqrt n r = (r+1)*(r+1) > n && r * r <= n
ordintsqrt n r =
    case ((r+1)*(r+1) <= n , r * r <= n) of
    (True ,True)  -> LT
    (False,False) -> GT
    (False,True ) -> EQ
    (_    ,    _) -> undefined

ordintproot p n r =
    case (((r+1)^p) <= n,(r^p) <= n) of
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

