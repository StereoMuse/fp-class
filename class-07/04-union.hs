{-
  В параметрах командной строки указаны имена текстовых файлов, содержащих целые числа, разделённые
  пробелами и символами перевода строк. Определить количество и сумму различных чисел, встречающихся
  в заданных текстовых файлах.
  
  :main ["a.txt", "b.txt"]
  
-}

import System.Environment
import System.IO
import System.Environment
import System.Directory
import Data.Char
import qualified Data.IntSet as Set

readNumFile :: FilePath -> IO [Int]
readNumFile file = do
	pt <- openFile file ReadMode
	content <- hGetContents pt
	return $ map read $ concatMap words $ lines content

solve ::[[Int]] -> (Int, Int)
solve xs = (length x, sum (x))
	where
	x = Set.toList $ foldl1 Set.union $ map Set.fromList xs

main = getArgs >>= mapM readNumFile >>= print.solve
