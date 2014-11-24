{-
	3. Пользуясь монадой State, реализовать функции для работы с очередью: enqueue и dequeue.
-}

import Control.Monad.State
type Queue = [Int]

enqueue x = do
	xs <- get
	put (xs ++ [x])

dequeue = do
	(x:xs) <- get
	put xs
	return x

test = do
	q <- dequeue
	enqueue q
	q <- dequeue
	enqueue q

-- проверки (true)
test1 = ((execState test [1, 2, 3]) == [3, 1, 2])
test2 = ((execState test [1, 2, 3, 4, 5, 6, 7]) == [3, 4, 5, 6, 7, 1, 2])