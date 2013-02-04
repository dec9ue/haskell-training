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

intsqrt n = head [y | y <- [1..n] , (y+1)*(y+1) > n, y *y<=n]

fact n = filter $ \m' -> n `mod` m' == 0

