{- 1. Написать программу, работа которой управляется конфигурационным файлом, содержащим строки следующего формата:
имя поля=значение
Возможными именами полей являются summand (слагаемое), multiplier (множитель), divisor (делитель). Все значения
являются целыми числами. В качестве параметров командной строки программе подаются имя конфигурационного файла
(config.txt) и имя текстового файла с целочисленными данными(numbers.txt). Над каждым целым числом из второго файла выполняются операции,
указанные в конфигурационном файле, то есть число складывается, умножается и делится соответственно.
Если какое-либо поле отсутствует, то действие не выполняется. Результаты вычислений выводятся на консоль.
Организовать доступ к параметрам конфигурационного файла средствами монады Reader. -}

import Control.Monad.Reader
import System.Environment

parseConfig str = do 
	let 
		(operation, num) = span (/= '=') str
	make operation ((\x -> read x :: Integer) . tail $ num)
		where
			make "summand" n = (+ n)
			make "multiplier" n = (* n)
			make "divisor" n = (`div` n)

main = do
	[fileConfig, fileNumbers] <- getArgs
	-- получаем числа
	nums <- (map (\x -> read x :: Integer) . lines) `fmap` readFile fileNumbers
	-- парсим каждую строку из файла config.txt
	conf <- (map parseConfig . lines) `fmap` readFile fileConfig
	print $ map (\x -> (runReader $ res x) conf) nums
		where
			res x = do
			actions <- ask
			return $ foldl (flip ($)) x actions 
			
{-
	пример вывода:
		*Main> :main config.txt numbers.txt
		Loading package transformers-0.3.0.0 ... linking ... done.
		Loading package mtl-2.1.3.1 ... linking ... done.
		[3,4,6]
-}