-- naive algorithm
maxCut :: [Int] -> Int -> Int
maxCut price 0 = 0
maxCut price n = maximum [price !! k + maxCut price (n-k) | k <-[1..n]]

--
maxCutSol :: [Int] -> Int -> (Int, [Int])
maxCutSol price 0 = (0,[])
maxCutSol price n = argMax fst [let (s,cuts) = maxCutSol price (n-k) in ( price !! k +s, k:cuts) | k <-[1..n]]

-- maximum element of the list if after a function
argMax :: Ord b => (a -> b) ->[a] -> a
argMax _ [x] = x
argMax f (x:xs) = let y = argMax f xs in 
                    if  (f x) > (f y) then x else y

--DP solution (Bottom-up method), exploiting lazy evaluation
maxCut2 ::[Int] -> Int -> Int
maxCut2 price n = last bestCut
                    where bestCut = 0 : [maximum [price !! i + bestCut !! (k-i) | i <- [1..k]] | k <-[1..n] ]


        