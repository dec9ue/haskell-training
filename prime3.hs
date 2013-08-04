isprime =  \a->(==[])$[x|x<-[2..7],mod a x==0]
main=return $ [2,3,5,7] ++ filter isprime [9..100]
