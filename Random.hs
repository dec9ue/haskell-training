import System.Random
import Control.Monad
import Control.Monad.Trans.State.Lazy

getRandomByte :: Int -> IO Int
getRandomByte max =  randomRIO (0,max-1)

evalDot :: (Int,Int) -> Int -> Ordering
evalDot (x,y) max
    | (x * x + y * y ) >= (max*max) = GT
    | (x+1)*(x+1) + (y+1)*(y+1) > max * max = EQ 
    | otherwise = LT

-- 
calcOneDotWith :: (Monad m)=>(Int -> m Int)->(Int,Int)->Int ->Int -> m (Ordering,(Int,Int,Int))
calcOneDotWith getRandomByte (lastx,lasty) max unit = do
    x <- (getRandomByte unit)
    y <- (getRandomByte unit)
    let (newx,newy) = (8*lastx+x,8*lasty+y)
        res         = evalDot (newx,newy) max
        in case res of
          EQ -> calcOneDotWith getRandomByte (newx,newy) ( 8 * max) unit
          _  -> return (res,(newx,newy,max))

calcOneDot :: IO (Bool,(Int,Int,Int))
calcOneDot  =  do
    (res,p) <- calcOneDotWith getRandomByte (0,0) 8 8
    case res of
        GT -> return (False,p)
        _  -> return (True ,p)

foldMN :: (Monad m)=>(a -> b -> b) -> m a -> Int -> b -> m b
foldMN folder func times init
  | times <= 0 = return init
  | otherwise  = do
     res <- func
     tail <-  foldMN folder func (times - 1) init
     return $ folder res tail

takeStat :: IO (Bool,(Int,Int,Int)) -> Int -> IO (Int,Int)
takeStat func count = 
    let c = (\(v,_) (a,b)->if v then (a+1,b+1) else (a,b+1))
    in
    foldMN c func count (0,0)

makeList :: IO a -> Int -> IO [a]
makeList func times = replicateM times func

-- instance Foldable IO where
--    foldr f z m = m >>= (return . ((flip f) z))

success :: IO Bool
success = return True
assert_msg :: String -> Bool -> IO Bool
assert_msg msg False = fail msg
assert_msg _ True    = success

assert :: Bool -> IO Bool
assert = assert_msg "test"

test :: IO Bool
test = do
   assert $ (== GT) $ evalDot (10,10) 10
   assert $ (== GT) $ evalDot (3,4) 5
   assert $ (== LT) $ evalDot (0,0) 5
   assert $ (== EQ) $ evalDot (3,3) 5
--   assert $ () $ calcOneDotWith getRand 
   success
