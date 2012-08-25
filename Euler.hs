problem_size = 9999

is_circle n = is_circle_sub n n 0
  where
    is_circle_sub m n r
      | m == 0    = r == n
      | otherwise = is_circle_sub (m `div` 10) n (r * 10 + m `mod` 10)

-- decendant list of pair (a,b) on value a + b
declist a b limit
  | a + b <= 2      = declist_sub a b limit []
  | (a+b-1) > limit = declist_sub a b limit $ declist limit (a + b -1 -limit) limit
  | otherwise       = declist_sub a b limit $ declist (a+b-2) 1 limit
  where
    declist_sub a b limit tail
      | a >= b    = (a,b):(declist_sub (a-1) (b+1) limit tail )
      | otherwise = tail

-- CAF avoids Garbage Collection
mult_list        = map (\ (x,y) -> x * y) $ declist problem_size problem_size problem_size 

main = do
  print $ maximum $ filter is_circle (take (search_limit-index+1) $ drop (index-1) mult_list)
    where
      (index,first_sol) = head $ filter (is_circle.snd) $ zip [1..] mult_list
      root              = toInteger $ floor $ sqrt $ fromInteger first_sol
      search_limit      = fromInteger $ (\x->x * x) $ (\x -> (problem_size - x +1)) $ root

