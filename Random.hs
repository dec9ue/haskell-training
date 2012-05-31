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
    (res,p) <- calcOneDotWith getRandomPair 256 (0,0,256)
    case res of
        GT -> return (False,p)
        _  -> return (True ,p)

-- PUBLIC:
-- take statistics of the count number of results.
-- statistics is formatted as (cout_of_true,total)
takeStat :: IO (Bool,(Int,Int,Int)) -> Int -> IO (Int,Int)
takeStat = takeStat1
takeStat1 func count =
    let c   = \(x1,y1)(x2,y2) -> (x1+x2,y1+y2)
        f   = \ (x,_) -> if x then (1,1) else (0,1)
    in
    foldBN' c (liftM f func) count

takeStat2 func count =
   let c = \(v,_) (a,b)->if v then (a+1,b+1) else (a,b+1)
   in
   foldMN' c func count (0,0)

-- PUBLIC:
-- lists all the result
makeList :: IO a -> Int -> IO [a]
makeList func times = replicateM times func

-- evaluates single point is intent of a circle.
--  GT means outside (or on-the-boundary) of the circle
--  EQ means lacking accuracy and gaining accuracy is needed.
--  LT means inside of the circle
evalDot :: (Int,Int,Int) -> Ordering
evalDot (x,y,max)
    | (x * x + y * y ) >= (max*max) = GT
    | (x+1)*(x+1) + (y+1)*(y+1) > max * max = EQ 
    | otherwise = LT

-- random function
getRandomPair :: Int -> IO (Int,Int)
getRandomPair max = do
   v <- randomRIO (0,max*max-1)
   return (v `mod` max,v `div` max)

-- generates points with the judgement (subroutine)
calcOneDotWith :: (Int -> IO (Int,Int))->Int->(Int,Int,Int)->IO (Ordering,(Int,Int,Int))
calcOneDotWith getRandomPair unit (lastx,lasty,max) = do
    (x,y) <- (getRandomPair unit)
    let (newx,newy) = (unit*lastx+x,unit*lasty+y)
        res         = evalDot (newx,newy,max)
        in case res of
            EQ -> calcOneDotWith getRandomPair unit (newx,newy,unit*max)
            _  -> return (res,(newx,newy,max))

-- similar to foldr', but with a limited length
foldMN' :: (Monad m)=>(a -> b -> b) -> m a -> Int -> b -> m b
foldMN' folder func times init =
    let foldsub folder func times v = if times <= 0
          then return v
          else do w <- func
                  foldsub folder func (times -1) $! folder w v
    in foldsub folder func times init

-- foldMN folder func times init
--      | times <= 0 = return init
--      | otherwise  = liftM2 folder func $ foldMN folder func (times - 1) init
-- 

-- similar to foldr1', but with a limited length and for monoid operation
foldBN' :: (Monad m)=>(a->a->a) -> m a -> Int -> m a
foldBN' folder m count
    | count == 1 = m
    | otherwise  = let c1 = count `div` 2 in do
          v1 <- foldBN' folder m c1
          v2 <- foldBN' folder m (count - c1)
          let! (v1',v2') = (v1,v2) in return $ folder v1' v2'

