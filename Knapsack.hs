
-- solves knapsack problem using dynamic planning.
solve_knapsack :: Int -> [(Int,Int)] -> (Int,[(Int,Int)])
solve_knapsack bagsize items = list_knapsack bagsize items !! bagsize

-- lists all the solutions
list_knapsack :: Int -> [(Int,Int)] -> [(Int,[(Int,Int)])]
list_knapsack bagsize items = foldr step (repeat (0,[])) items

-- evaluates single item.
step :: (Int,Int)-> [(Int,[(Int,Int)])] -> [(Int,[(Int,Int)])]
step item@(_,w) list = 
   (take w list) ++ ( zipWith (step_minor item) list $ drop w list)

-- calculates single cell of the DP
step_minor :: (Int,Int) -> (Int,[(Int,Int)]) -> (Int,[(Int,Int)]) -> (Int,[(Int,Int)])
step_minor item@(v,_) (v1,h1) (v2,h2) =
   if v1 + v > v2 then (v1+v,item:h1) else (v2,h2)

