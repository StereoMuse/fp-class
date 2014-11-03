{-
  Создать текстовый файл, содержащий случайные целые числа, разделённые пробелами
  и символами перевода строки. В командной строке должны задаваться следующие параметры:
  1) имя создаваемого файла; filename
  2) диапазон генерируемых случайных чисел: от и до; bounds = min, max
  3) количество чисел в строке; countNumbers
  4) количество строк в файле. countRows
  
  -- liftM :: Monad m => (a1 -> r) -> m a1 -> m r
  -- replicateM :: Monad m => Int -> m a -> m (Seq a)

  :main "a.txt" 10 100 2 10
-}

import System.Environment
import System.Random
import Data.List
import Control.Monad

func2 countNumbers bounds = do
-- случайная посл-сть
	gen <- newStdGen
-- посл-сть с пробелами - (countNumbers) чисел
	return $ unwords $ map show $ take countNumbers (randomRs bounds gen :: [Int])

func1 countNumbers countRows bounds = do
-- текст в одну строку - (countRows) строк
	text <- liftM unlines $ replicateM countRows $ func2 countNumbers bounds
	return text

main = do
-- считали аргументы с ком.строки
	[filename, min, max, countNumbers, countRows] <- getArgs
-- вызвали func1, результат - строка - в text
	text <- func1 (read countNumbers :: Int) (read countRows :: Int) (read min :: Int, read max :: Int)
-- создали файл с текстом из строки text
	writeFile filename text
