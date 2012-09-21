import System.Random
import System.IO.Unsafe

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
            putChar '\n'
            return True
        else do
            a <- randomRIO ( 1, r-1) 
            ires <- return $ rabbintest a k r
            putChar '+'
            case ires of
                False -> print "*\n" >> return False
                True  -> millerrabbin_sub r t (i+1)
  in
  millerrabbin_sub r t 1

find_next_prime n = 
  do
    res <- millerrabbintest n 10
    case res of
      True  -> return n
      False -> find_next_prime (n + 1)

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

