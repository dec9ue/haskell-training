import Control.Monad.Trans.State.Lazy

tick :: State Int Int
tick = do n <- get
          put (n+1)
          m <- get
          put (m+2)
          return m

tick2 :: State Int [Int]
tick2 = do n <- get
           put $ n + 4
           put $ n + 5
           modify $ \x -> x + 3
           m <- get
           return [n]

buf :: StateT Int [] Int
buf = do n <- get
         put (n+1)
         m <- get
         put (m+2)
         return m




