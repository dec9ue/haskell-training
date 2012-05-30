-- calculates pi/4 with Monte Carlo simulatoin
-- takeStat calcOneDot 1000
-- will obtain a statistics of 1000 samples of points in the style of (matched,samples)
-- as the samples increases, (matched / samples) approaches to pi/4 slowly. 
import System.Random
import Control.Monad

-- PUBLIC:
-- generates points with the judgement 
calcOneDot :: IO (Bool,(Int,Int,Int))
calcOneDot  =  do
    (res,p) <- calcOneDotWith getRandomByte 256 (0,0,256)
    case res of
        GT -> return (False,p)
        _  -> return (True ,p)

-- PUBLIC:
-- take statistics of the count number of results.
-- statistics is formatted as (cout_of_true,total)
takeStat :: IO (Bool,(Int,Int,Int)) -> Int -> IO (Int,Int)
takeStat func count = 
    let c = \(v,_) (a,b)->if v then (a+1,b+1) else (a,b+1)
    in
    foldMN c func count (0,0)

-- PUBLIC:
-- lists all the result
makeList :: IO a -> Int -> IO [a]
makeList func times = replicateM times func

-- random function
getRandomByte :: Int -> IO Int
getRandomByte max =  randomRIO (0,max-1)

-- evaluates single point is intent of a circle.
--  GT means outside (or on-the-boundary) of the circle
--  EQ means lacking accuracy and gaining accuracy is needed.
--  LT means inside of the circle
evalDot :: (Int,Int,Int) -> Ordering
evalDot (x,y,max)
    | (x * x + y * y ) >= (max*max) = GT
    | (x+1)*(x+1) + (y+1)*(y+1) > max * max = EQ 
    | otherwise = LT

-- generates points with the judgement (subroutine)
calcOneDotWith :: (Int -> IO Int)->Int->(Int,Int,Int)->IO (Ordering,(Int,Int,Int))
calcOneDotWith getRandomByte unit (lastx,lasty,max) = do
    x <- (getRandomByte unit)
    y <- (getRandomByte unit)
    let (newx,newy) = (unit*lastx+x,unit*lasty+y)
        res         = evalDot (newx,newy,max)
        in case res of
            EQ -> calcOneDotWith getRandomByte unit (newx,newy,unit*max)
            _  -> return (res,(newx,newy,max))

-- similar to foldr, but with a limited length
foldMN :: (Monad m)=>(a -> b -> b) -> m a -> Int -> b -> m b
foldMN folder func times init
     | times <= 0 = return init
     | otherwise  = liftM2 folder func $ foldMN folder func (times - 1) init

