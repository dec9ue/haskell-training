-- Simple example for Bang-Patterns leak

import Control.Monad
import System.IO.Unsafe ( unsafePerformIO )
import Control.DeepSeq
import Data.Foldable ( foldr' )
import System.IO

main :: IO ()
main = do
    result <- takeStat (return 100) 10000000
    print $ "result is " ++ show result

takeStat :: IO Int -> Int -> IO Int
takeStat io count =
    return $ foldr'' ((+).unsafePerformIO) 0 $ replicate count io

foldr''  :: NFData b => (a->b->b) -> b-> [a] -> b
foldr'' folder init []          = init
foldr'' folder init (head:tail) = 
    (foldr'' folder $!! folder head init) tail

