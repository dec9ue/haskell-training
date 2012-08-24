-- main = do
is_circle n      = (show n) == (reverse $ show n)
declist :: Integer -> Integer -> Integer ->  [(Integer,Integer)] ->  [(Integer,Integer)]
declist a b limit tail =
    if b <= a
    then (a,b):(declist (a-1) (b+1) limit tail )
    else tail
declist_rec a b limit  = declist a b limit $
    if(a + b > 2)
    then
      if (a+b-1) > limit 
      then declist_rec limit (a + b -1 -limit) limit
      else declist_rec (a+b-2) 1 limit
    else [] 
mult_list       :: [Integer]
mult_list        = [(x*y) | (x,y) <- declist_rec 999 999 999] 
first_sol        = head $ filter is_circle mult_list
search_limit     = (\x->x * (1 + x)) $ (\x -> (999 - x +1)) $ floor $ sqrt $ fromInteger first_sol

main = do
     print $ "first sol:" ++ show first_sol
     print $ show $ take 10 mult_list
     print $ "search limit:" ++ show search_limit
     print $ maximum $ filter is_circle (take search_limit mult_list)
 

