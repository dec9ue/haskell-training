-- calculates pi/4 with Monte Carlo simulatoin
-- takeStat calcOneDot 1000
-- will obtain a statistics of 1000 samples of points in the style of (matched,samples)
-- as the samples increases, (matched / samples) approaches to pi/4 slowly. 
import System.Random
import Control.Monad
import Data.Foldable ( foldr' )
import Data.IORef
import System.IO.Unsafe ( unsafePerformIO )
import System.IO( hFlush , stdout)


type Point = (Int,Int,Int)
type Stat  = (Int,Int)

main :: IO ()
main = do
    (count,total) <- takeStat calcOneDot 10000000
    print (count * 4,total)

-- PUBLIC:
-- generates points with the judgement 
calcOneDot :: IO (Bool,Point)
calcOneDot  =  do
    (res,p) <- calcOneDotWith 256 (0,0,256)
    case res of
        GT -> return (False,p)
        _  -> return (True ,p)


showSanity (r,(x,y,max))
    | r == res  = "sane"
    | r == GT && res == EQ = "sane"
    | otherwise = "INSANE" ++ show (r,(x,y,max))
    where res = (x*x + y*y) `compare` (max * max)

-- PUBLIC:
-- take statistics of the count number of results.
-- statistics is formatted as (cout_of_true,total)
takeStat :: IO (Bool,Point) -> Int -> IO Stat
takeStat = takeStat5

takeStat5 func count =
    let c   = \(x1,y1)(x2,y2) -> (x1+x2,y1+y2)
        f   = \ (x,_) -> if x then (1,1) else (0,1)
    in
    foldRN''' c (liftM f func) count (0,0)

-- evaluates single point is intent of a circle.
--  GT means outside (or on-the-boundary) of the circle
--  EQ means lacking accuracy and gaining accuracy is needed.
--  LT means inside of the circle
{-# INLINE evalDot #-}
evalDot :: Point -> Ordering
evalDot (x,y,max) 
    | xx + yy >= mm            = GT
    | xx + yy + 2*(x+y+1) > mm = EQ
    | otherwise                = LT
    where xx = x * x
          yy = y * y
          mm = max * max

-- IORef for custom random
tempIORef :: IORef ([Int])
tempIORef = unsafePerformIO $ newIORef $ randomRs (0,255) $ mkStdGen 100

getRandomIO :: IO Int
-- getRandomIO = return 255
getRandomIO = do
    list <- readIORef tempIORef
    writeIORef tempIORef $ tail list
    return $ head list

-- custom randomRIO
randomRIO_c :: (Int,Int) -> IO Int
randomRIO_c = randomRIO

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
    let newx        = unit*lastx+x
        newy        = unit*lasty+y
        res         = evalDot (newx,newy,max)
        in case res of
            EQ -> calcOneDotWith unit (newx,newy,unit*max)
            _  -> return (res,(newx,newy,max))

{-# INLINE foldRN''' #-}
foldRN''' :: (Show a)=> (a->a->a) -> IO a -> Int -> a -> IO a
foldRN''' folder m count init =
    let foldRN_sub''' folder m count v = if count == 0
            then v
            else
               let r = unsafePerformIO $ do
                   when ((count `mod` 100000) == 0) $ do
                      putStrLn $ "v : " ++  show v ++ " count :" ++ show count
                      hFlush stdout
                   m
               in
            foldRN_sub''' folder m (count -1) $! folder v r
    in return $ foldRN_sub''' folder m count init 



