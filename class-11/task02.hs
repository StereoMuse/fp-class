{- 
	2. Организовать вычисление значений функций sin и cos, пользуясь рядами Тейлора и 
	сохраняя каждое слагаемое в журнал посредством монады Writer. 
	В тексте программы допускается только один вызов функции tell.
-}

import Control.Monad.Writer
import System.Environment
eps = 1e-10

-- n - кол-во элементов ряда; i - текущий; x - аргумент; pr - предыдущий; res - результат; xs - список всех
mySin n i x pr res xs
	| i == 0 = mySin n 1 x x x (x:xs)
	| n == i = tell (reverse xs) >> return res
	| otherwise = 
			return ((/) (-pr *x**2 ) ((2*i+1)*2*i)) >>= \r -> mySin n (i+1) x r (res + r) (r: xs)

myCos n i x pr res xs
	| i == 0 = myCos n 1 x 1 1 (x:xs)
	| n == i = tell (reverse xs) >> return res
	| otherwise = 
			return ((/) (-pr *x**2 ) (2*i*(2*i-1))) >>= \r -> myCos n (i+1) x r (res + r) (r: xs)
			
-- проверки (true)
test1 = abs ((fst. runWriter $ mySin 10 0 (pi/2) 0 0 []) - sin (pi/2) ) < eps
test2 = abs ((fst. runWriter $ mySin 10 0 (pi/4) 0 0 []) - sin (pi/4) ) < eps

test3 = abs ((fst. runWriter $ myCos 10 0 (pi/2) 0 0 []) - cos (pi/2) ) < eps
test4 = abs ((fst. runWriter $ myCos 10 0 (pi/4) 0 0 []) - cos (pi/4) ) < eps
