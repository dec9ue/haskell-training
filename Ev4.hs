import Control.Parallel.Strategies

parListWithN :: Int -> [a] -> Eval ()
parListWithN n l = do
    parSeqListPar $ take n l
    case drop n l of
      [] -> return ()
      _  -> parListWithN n $ drop n l

parSeqListPar :: [a] -> Eval ()
parSeqListPar [] = return ()
parSeqListPar (hd:tl) = do
    rpar hd
    parSeqListPar tl
    rseq hd
    return ()

filterEvalN :: (a -> Bool) -> [a] -> [a]
filterEvalN f l = runEval $ do
    rpar $ runEval $ parListWithN 100 l
    return $ filter f l

isPrime :: Integral b => b -> Bool
isPrime n = not $ or $ map ((==0).(n `mod`)) [2..(n-2)]

-- main = print $ take 100 $ filterEvalN isPrime [43532466234562353523..]
main = print $ take 100 $ filterEvalN isPrime [435300..]
-- main = print $ take 100 $ filterEvalN isPrime [3..]

