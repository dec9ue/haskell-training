import Control.Parallel.Strategies

evalParListWithN n l = do
    x <- parListN n rpar l
    case drop n l of
      [] -> return []
      _  -> evalParListWithN n $ drop n l

filterEvalN f l = runEval $ do
    x <- rpar $ evalParListWithN 10 l
    return $ filter f l

isPrime n = not $ or $ map ((==0).(n `mod`)) [2..(n-2)]

-- main = print $ take 100 $ filterEvalN isPrime [43532466234562353523..]
main = print $ take 100 $ filterEvalN isPrime [435300..]
-- main = print $ take 100 $ filterEvalN isPrime [3..]

