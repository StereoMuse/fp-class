{-
  В параметрах командной строки указаны имена текстовых файлов, содержащих целые числа, разделённые
  пробелами и символами перевода строк. Определить количество и вывести различные числа, встречающиеся
  в каждом из заданных текстовых файлов. Указание: в решении следует воспользоваться множествами.

  :main "a.txt"
  
  -}

import System.Environment
import System.IO
import System.Environment
import System.Directory
import Data.Char
import qualified Data.IntSet as Set

readNumFile :: FilePath -> IO [Int]
readNumFile file = do
-- файл открыт на чтение
	pt <- openFile file ReadMode
-- returns the list of characters corresponding to the unread portion of file
	content <- hGetContents pt
	return $ map read $ concatMap words $ lines content

solve ::[[Int]] -> (Int, [Int])
solve xs = (length x, x)
	where
	-- ищем различия списков
		x = Set.toList $ foldl1 Set.intersection $ map Set.fromList xs

main = getArgs >>= mapM readNumFile >>= print.solve 