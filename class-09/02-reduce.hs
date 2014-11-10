import System.Environment
import System.Random
import Data.Maybe

{-
  Напишите функцию reduce, принимающую один целочисленный аргумент a и возвращающую 
  0, если аргумент делится на 3, 
  a^2, если он на 3 не делится и является при этом нечётным,
  a^3 в остальных случаях.
-}

reduce :: Integral a => a -> a
reduce a
	| a `mod` 3 == 0 = 0
	| odd a = a^2
	| otherwise = a^3

{-
  Напишите функцию, применяющую функцию reduce заданное количество раз к 
  значению в контексте, являющемся функтором:
-}

reduceNF :: (Functor f, Integral a) => Int -> f a -> f a
reduceNF 0 a = a
reduceNF n a = reduceNF (n-1) (fmap reduce a)

{-
  Реализуйте следующие функции-преобразователи произвольным, но, желательно, 
  осмысленным и нетривиальным способом.
-}

toList :: Integral a => [(a, a)]  -> [a]
toList = foldr (\(x,y) acc -> (x + y + 42) : acc) []

toMaybe :: Integral a => [(a, a)]  -> Maybe a
toMaybe [] = Nothing
toMaybe xs = Just (maximum  $ toList xs)

toEither :: Integral a => [(a, a)]  -> Either String a
toEither [] = Left "Nothing"
toEither xs = Right (maximum $ toList xs)

-- воспользуйтесь в этой функции случайными числами
toIO :: Integral a => [(a, a)]  -> IO a
toIO xs = do
	gen <- newStdGen
	let (i, genn) = randomR (0,length xs -1 ) gen::(Int,StdGen)
	let (f,s) = (!!) xs i
	let xss = if f > s then [s..f] else [f..s]
	let (ii, gen) = randomR (0,length xss-1) genn::(Int,StdGen)
	return $ (!!) xss ii
	
{-
  В параметрах командной строки задано имя текстового файла, в каждой строке
  которого записана пара целых чисел, разделённых пробелами. Загрузите
  данные из файла в список пар целых чисел, преобразуйте этот список к
  значениям в контекстах [], Maybe, Either String и IO и примените к каждому
  из контекстов функцию reduceNF (значение N также должно браться из 
  параметров командной строки).
-}

parseArgs :: [String] -> (FilePath, Int)
parseArgs s = (head s, read $ last s)

readData :: FilePath -> IO [(Int, Int)]
readData filename = do
	content <- readFile filename
	return $ map (\s -> let [f',s'] = words s in (read f', read s')) $ lines content

main = do
  (filename, n) <- parseArgs `fmap` getArgs
  ps <- readData filename
  print $ toList ps
  print $ reduceNF n (toList ps)
  print $ reduceNF n (toMaybe ps)
  print $ reduceNF n (toEither ps)
  reduceNF n (toIO ps) >>= print

{-
  Подготовьте несколько тестовых файлов, демонстрирующих особенности различных контекстов.
  Скопируйте сюда результаты вызова программы на этих файлах.

файл "b.txt":
	2 5
	4 8
	2 6
	4 4
	1 3
	8 2
	9 1

результат:
	*Main> :main "b.txt" 0
	[49,54,50,50,46,52,52]
	[49,54,50,50,46,52,52]
	Just 54
	Right 54
	5
-}
