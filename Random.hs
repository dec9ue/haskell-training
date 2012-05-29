import System.Random
import Control.Monad

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
    let (newx,newy) = (8*lastx+x,8*lasty+y)
        res         = evalDot (newx,newy,max)
        in case res of
          EQ -> calcOneDotWith getRandomByte unit (newx,newy,8*max)
          _  -> return (res,(newx,newy,max))

-- generates points with the judgement 
calcOneDot :: IO (Bool,(Int,Int,Int))
calcOneDot  =  do
    (res,p) <- calcOneDotWith getRandomByte 8 (0,0,8)
    case res of
        GT -> return (False,p)
        _  -> return (True ,p)

-- similar to foldr, but with a limited length
foldMN :: (Monad m)=>(a -> b -> b) -> m a -> Int -> b -> m b
foldMN folder func times init
  | times <= 0 = return init
  | otherwise  = do
     res <- func
     tail <-  foldMN folder func (times - 1) init
     return $ folder res tail

-- take statistics of the count number of results.
-- statistics is formatted as (cout_of_true,total)
takeStat :: IO (Bool,(Int,Int,Int)) -> Int -> IO (Int,Int)
takeStat func count = 
    let c = \(v,_) (a,b)->if v then (a+1,b+1) else (a,b+1)
    in
    foldMN c func count (0,0)

-- lists all the result
makeList :: IO a -> Int -> IO [a]
makeList func times = replicateM times func
