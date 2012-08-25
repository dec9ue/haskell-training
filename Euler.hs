
is_circle n = is_circle_sub n n 0
    where
        is_circle_sub m n r
            | m == 0 = r == n
            | otherwise = is_circle_sub (m `div` 10) n (r * 10 + m `mod` 10)

declist a b limit tail
    | b <= a    = (a,b):(declist (a-1) (b+1) limit tail )
    | otherwise = tail

-- decendant list of pair (a,b) on value a + b
declist_rec a b limit
    | a + b <= 2      = declist a b limit []
    | (a+b-1) > limit = declist a b limit $ declist_rec limit (a + b -1 -limit) limit
    | otherwise       = declist a b limit $ declist_rec (a+b-2) 1 limit

mult_list        = map (\ (x,y) -> x * y) $ declist_rec 9999 9999 9999 
index_mult_list  = zip [1..] mult_list

main =
    let (index,first_sol) = head $ filter (is_circle.snd) index_mult_list
        root              = floor $ sqrt $ fromInteger first_sol
        search_limit      = (\x->x * x) $ (\x -> (9999 - x +1)) $ root
    in
    do
        print $ maximum $ filter is_circle (drop (index-1) $ take search_limit mult_list)

