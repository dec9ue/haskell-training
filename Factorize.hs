import Data.Maybe
import System.Random
import System.IO.Unsafe
import Control.Monad
import Control.Parallel.Strategies
import System.IO

prime_limit = 100000

display s = do
    putStrLn s
    hFlush stdout

main =
  do
    display $ "preparing primes..."
    display $ "last element is : " ++ (show $ last $ take prime_limit $ evaluate_parallel $ prime_seq)
    display $ "preparing primes...done."
    forM_ (take 10 $ sieve_seq 888888888888888888884327) (\x -> display $ show x)
    display "now start"
    forM_ (sieve_seq  280671392065546467397265294532969672241810318954163887187279320454220348884327) (\x -> display $ show x)

-- evaluate_parallel = parMap rseq id
evaluate_parallel = withStrategy $ parListNth 5 rdeepseq

sieve_seq n = do
    x <- [(1 + intsqrt n)..]
    r <- return $ x * x - n
    prime_list <- return $ take prime_limit prime_seq
    res <- return $ evaluate_parallel $ limited_factorize_with_list r prime_list
    if all (<=(last prime_list)) res
    then [(x,r,res)]
    else []

limited_factorizable n limit =  limited_factorizable_with_list n $ take limit prime_seq

limited_factorizable_with_list n prime_list = all (<= (last prime_list)) $ limited_factorize_with_list n prime_list

limited_factorize n limit = limited_factorize_with_list n $ take limit prime_seq

limited_factorize_with_list 1 prime_list = []
-- limited_factorize_with_list n prime_list =
--    let fact_list  = fact n prime_list in
--    let rest_val   = n `div` product fact_list in
--    if rest_val == n
--    then fact_list ++ [n]
--    else fact_list ++ (limited_factorize_with_list rest_val fact_list)
limited_factorize_with_list n prime_list =
   limited_factorize_with_list_internal n prime_list prime_list []

limited_factorize_with_list_internal n prime_list [] [] = [n]

limited_factorize_with_list_internal n prime_list [] res_list =
   limited_factorize_with_list_internal n res_list res_list []

limited_factorize_with_list_internal n prime_list (cur_head:cur_tail) res_list =
   if n `mod` cur_head == 0
   then cur_head:(limited_factorize_with_list_internal (n `div` cur_head) prime_list cur_tail (cur_head:res_list))
   else limited_factorize_with_list_internal n prime_list cur_tail res_list

factorize n =
    case p of
    Nothing -> [n]
    Just p' -> p' : factorize (n `div` p')
    where
        p = listToMaybe $ fact n [2..m]
        m = intsqrt n

-- intsqrt n = head [y | y <- [1..n] , (y+1)*(y+1) > n, y *y<=n]
intsqrt n = binsearch (ordintsqrt n) 1 n

fact n = filter $ \m' -> n `mod` m' == 0

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

-- debug = id
debug = \x -> return ()

modexp m p n = modexp_sub m p n m 1
  where
    modexp_sub m p n c r
      | p == 0         = r
      | otherwise      =
         let res = if ( p `mod` 2 == 1 ) then (c * r) `mod` n else r in
         modexp_sub m (p `div`2) n ((c * c)`mod` n) res

find_s_k r = find_s_k_sub (r-1) 1 1
  where
    find_s_k_sub r ss s
      | (r`div` ss) `mod` 2 == 1  = (s,r `div` ss)
      | otherwise                 = find_s_k_sub r (2*ss) (1+ s)

prime_prod limit = prime_prod_sub limit 2 1
  where
    prime_prod_sub limit cur res
      | res*cur > limit = res
      | unsafePerformIO $ millerrabbintest cur 10
                        = prime_prod_sub limit (cur + 1) (res*cur)
      | otherwise       = prime_prod_sub limit (cur + 1) res

millerrabbintest r t =
  let (s,k) = find_s_k r in
  let rabbintest_sub j b' =
        if (b' == r-1)||(b' == 1)
        then True
        else
            if j < s
            then rabbintest_sub (j+1)((b'*b')`mod` r) 
            else False
  in
  let rabbintest a k r = rabbintest_sub 0 $ modexp a k r in
  let millerrabbin_sub r t i =
        if (i > t )
        then do
            debug $ putChar '\n'
            return True
        else do
            a <- randomRIO ( 1, r-1) 
            ires <- return $ rabbintest a k r
            debug $ putChar '+'
            case ires of
                False -> (debug $ print "*\n") >> return False
                True  -> millerrabbin_sub r t (i+1)
  in
  millerrabbin_sub r t 1

find_next_prime n = 
  do
    res <- millerrabbintest n 10
    case res of
      True  -> return n
      False -> find_next_prime (n + 1)

prime_seq :: [Integer]
prime_seq = filter (\n -> unsafePerformIO $ millerrabbintest n 10) [2..]

relative_prime p q
  | p == 0 || q == 0 = False
  | p == 1 || q == 1 = True
  | p > q            = relative_prime q (p `mod` q)
  | otherwise        = relative_prime p (q `mod` p)

find_prime keylen = 
  let randomv = let min = 2 ^ (keylen - 1) in
                randomRIO ( min,2*(min-1))
  in do
    v <- randomv
    find_next_prime v

