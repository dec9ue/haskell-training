import Data.Maybe

main =
  do
    print $ factorize 1024
    putStrLn ""
    print $ factorize 280671392065546467397265294532969672241810318954163887187279320454220348884327

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

binsearchIO ismatch high low = do
    print $ "binsearch io : " ++ show high ++ "," ++ show low
    case (ismatch high, ismatch low) of
      (EQ, _)  -> return high
      ( _,EQ)  -> return low
      _        -> let mid = low+((high-low) `div` 2)  in
        case ismatch mid of
        EQ -> return mid
        GT -> binsearchIO ismatch mid low
        LT -> binsearchIO ismatch high mid

