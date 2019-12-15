--Insertion Sort, runs in O(n^2)
insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) = if x <= y then x:y:ys else insert x ys

ins_sort :: Ord a => [a] -> [a]
ins_sort [] =[]
ins_sort (x:xs) = insert x (ins_sort xs)

--other equivalent formulations
ins_sort2 :: Ord a => [a] -> [a]
ins_sort2 = foldr insert []

ins_sort3 :: Ord a => [a] -> [a]
ins_sort3 = foldl (flip insert) []
 

--Quick Sort, best case: O(n(log n)) / worth case: O(n^2) 
quick_sort :: Ord a => [a] -> [a]
quick_sort (x:xs) = quick_sort smaller xs ++ [x] ++ quick_sort larger xs
                where smaller = [a | a <- xs, a <= x]
                      larger  = [b | b <- xs, b > x]


--Merge Sort, runs in O(n(log n))
merge :: Ord a => [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge xs@(x:xs') ys@(y:ys') = if x<=y then x : merge xs' ys
                                      else y : mrege xs ys'

merge_sort :: Ord a => [a]
merge_sort [] = []
merge_sort xs = let (ys,zs) = splitAt (length xs `div` 2) xs
                in merge (merge_sort ys) (merge_sort zs)

