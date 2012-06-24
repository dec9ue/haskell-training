-- calculates pi/4 with Monte Carlo simulatoin
-- takeStat calcOneDot 1000
-- will obtain a statistics of 1000 samples of points in the style of (matched,samples)
-- as the samples increases, (matched / samples) approaches to pi/4 slowly. 
import System.Random
import Control.Monad
import Control.DeepSeq
import Data.Foldable ( foldl' )
import System.IO.Unsafe ( unsafePerformIO )
import System.IO


type Point = (Int,Int,Int)
type Stat  = (Int,Int)

main :: IO ()
main = do
    (count,total) <- takeStat calcOneDot 10000000
    print (count * 4,total)

-- take statistics of the count number of results.
-- statistics is formatted as (cout_of_true,total)
takeStat :: IO (Bool,Point) -> Int -> IO Stat
takeStat func count = 
    foldLN1 sum_stat (liftM point_to_stat func) count (0,0)

-- uses deepseq (it works!)
foldLN1 :: (NFData a) =>(a->b->a) -> IO b -> Int -> a -> IO a
foldLN1 folder m count init =
    let foldLN_sub folder m count v = if count == 0 
        then v
        else let v' = unsafePerformIO m 
              in foldLN_sub folder m (count -1) $!! folder v v'
    in return $ foldLN_sub folder m count init 

-- uses bang+print (it works! but needs thunk consumption using print)
foldLN2 :: (Show a) =>(a->b->a) -> IO b -> Int -> a -> IO a
foldLN2 folder m count init =
    let foldLN_sub folder m count v = if count == 0 
        then v
        else let v' = unsafePerformIO $ do
                 when (count `mod` 100000 == 0) $ print_stat v count
                 m 
              in foldLN_sub folder m (count -1) $! folder v v'
    in return $ foldLN_sub folder m count init 

-- uses standard bang (foldl' : it doesn't work)
foldLN3 :: (a->b->a) -> IO b -> Int -> a -> IO a
foldLN3 folder m count init =
    return $ foldl' (\x y-> folder x $ unsafePerformIO y) init $ replicate count m

-- uses standard bang + force (it works!)
foldLN4 :: (NFData a)=>(a->b->a) -> IO b -> Int -> a -> IO a
foldLN4 folder m count init =
    return $ foldl' (\x y -> force $ folder x $ unsafePerformIO y) init $ replicate count m

print_stat v count = do
    putStrLn $ "value : " ++ show v ++ " count : " ++ show count 
    hFlush stdout

-- util functions
point_to_stat :: (Bool,a) -> Stat
point_to_stat (b,_) = if b then (1,1) else (0,1)

sum_stat :: Stat -> Stat -> Stat
sum_stat (v1,t1) (v2,t2) = (v1+v2,t1+t2)


-- generates points with the judgement 
calcOneDot :: IO (Bool,Point)
calcOneDot  =  do
    (res,p) <- calcOneDotWith 256 (0,0,256)
    case res of
        GT -> return (False,p)
        _  -> return (True ,p)

-- evaluates single point is intent of a circle.
--  GT means outside (or on-the-boundary) of the circle
--  EQ means lacking accuracy and gaining accuracy is needed.
--  LT means inside of the circle
evalDot :: Point -> Ordering
evalDot (x,y,max)
    | (x * x + y * y ) >= (max*max) = GT
    | (x+1)*(x+1) + (y+1)*(y+1) > max * max = EQ 
    | otherwise = LT

-- random function
getRandomPair :: Int -> IO Stat
getRandomPair max = do
    v <- randomRIO (0,max*max-1)
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
