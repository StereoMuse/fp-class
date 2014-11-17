{- Пользуясь списком как монадой, вычислите пересечение заданных списков -}

import Control.Monad

intersect :: Eq a => [[a]] -> [a]
intersect [] = []
intersect (x : xs) = foldr (\acc y -> return y >>= filter (`elem` acc)) x xs

{-  test1
	test2
-}
test1 = intersect [[1],[2]]
test2 = intersect [[1,2],[2,3]]
