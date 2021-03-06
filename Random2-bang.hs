-- calculates pi/4 with Monte Carlo simulatoin
-- takeStat calcOneDot 1000
-- will obtain a statistics of 1000 samples of points in the style of (matched,samples)
-- as the samples increases, (matched / samples) approaches to pi/4 slowly. 
import System.Random
import Control.Monad
import Data.Foldable ( foldr' )
import Data.IORef
import System.IO.Unsafe ( unsafePerformIO )


type Point = (Int,Int,Int)
type Stat  = (Int,Int)

main :: IO ()
main = do
    (count,total) <- takeStat calcOneDot 10000000
    print (count * 4,total)

-- main = do
--     res <- replicateM 10 getRandomIO
--     print res


-- PUBLIC:
-- generates points with the judgement 
calcOneDot :: IO (Bool,Point)
calcOneDot  =  do
    (res,p) <- calcOneDotWith 256 (0,0,256)
    case res of
        GT -> return (False,p)
        _  -> return (True ,p)

-- PUBLIC:
-- take statistics of the count number of results.
-- statistics is formatted as (cout_of_true,total)
takeStat :: IO (Bool,Point) -> Int -> IO Stat
takeStat = takeStat5
takeStat1 func count =
    let c   = \(x1,y1)(x2,y2) -> (x1+x2,y1+y2)
        f   = \ (x,_) -> if x then (1,1) else (0,1)
    in
    foldBN' c (liftM f func) count

takeStat2 func count =
    let c = \(v,_) (a,b)->if v then (a+1,b+1) else (a,b+1)
    in
    foldMN' c func count (0,0)

takeStat3 func count =
    let c   = \(x1,y1)(x2,y2) -> (x1+x2,y1+y2)
        f   = \ (x,_) -> if x then (1,1) else (0,1)
    in
    foldRN' c (liftM f func) count (0,0)

takeStat4 func count =
    let c   = \(x1,y1)(x2,y2) -> (x1+x2,y1+y2)
        f   = \ (x,_) -> if x then (1,1) else (0,1)
    in
    foldMRN' c (liftM f func) count (0,0)

takeStat5 func count =
    let c   = \(x1,y1)(x2,y2) -> (x1+x2,y1+y2)
        f   = \ (x,_) -> if x then (1,1) else (0,1)
    in
    foldRN''' c (liftM f func) count (0,0)

-- PUBLIC:
-- lists all the result
makeList :: IO a -> Int -> IO [a]
makeList func times = replicateM times func

-- evaluates single point is intent of a circle.
--  GT means outside (or on-the-boundary) of the circle
--  EQ means lacking accuracy and gaining accuracy is needed.
--  LT means inside of the circle
evalDot :: Point -> Ordering
evalDot (x,y,max)
    | (x * x + y * y ) >= (max*max) = GT
    | (x+1)*(x+1) + (y+1)*(y+1) > max * max = EQ 
    | otherwise = LT

-- IORef for custom random
tempIORef :: IORef ([Int])
tempIORef = unsafePerformIO $ newIORef $ randomRs (0,255) $ mkStdGen 100

getRandomIO :: IO Int
getRandomIO = return 255
-- getRandomIO = do
--     list <- readIORef tempIORef
--     writeIORef tempIORef $ tail list
--     return $ head list

-- custom randomRIO
randomRIO_c :: (Int,Int) -> IO Int
randomRIO_c _ = getRandomIO

-- random function
getRandomPair :: Int -> IO Stat
getRandomPair max = do
    v <- randomRIO_c (0,max*max-1)
    return (v `mod` max,v `div` max)

-- generates points with the judgement (subroutine)
calcOneDotWith :: Int->Point->IO (Ordering,Point)
calcOneDotWith unit (lastx,lasty,max) = do
    x <- randomRIO (0,unit-1)
    y <- randomRIO (0,unit-1)
    let (newx,newy) = (unit*lastx+x,unit*lasty+y)
        res         = evalDot (newx,newy,max)
        in case res of
            EQ -> calcOneDotWith unit (newx,newy,unit*max)
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
        return $! folder v1 v2

--
foldRN' :: (a->a->a) -> IO a -> Int -> a -> IO a
foldRN' folder m count init =
    let f (c,x) y = if c > 0 then folder x y else  y
    in return $ foldr' f init [(count -i , unsafePerformIO m)| i <- [1..]]

foldRN'' :: (a->a->a) -> IO a -> Int -> a -> IO a
foldRN'' folder m count init =
    return $ foldr' folder init $ replicate count $ unsafePerformIO m

{-# INLINE foldRN''' #-}
foldRN''' :: (a->a->a) -> IO a -> Int -> a -> IO a
foldRN''' folder m count init =
    let foldRN_sub''' folder m count v = if count == 0
            then v
            else 
                let ! r = folder v $ unsafePerformIO m 
                in foldRN_sub''' folder m (count -1) r
    in return $ foldRN_sub''' folder m count init 

foldMRN' :: (a->a->a) -> IO a -> Int -> a -> IO a
foldMRN' folder m count init =
    let f = liftM2 folder
    in foldr' f (return init) $ replicate count m
