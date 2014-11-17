{-
   Дан текстовый файл, содержащий данные о нескольких студентах в следующем формате: три последовательные
   строки соответствуют имени, возрасту и номеру группы (например, 4.9). Определить соответствующий тип
   данных (data) и организовать чтение файла в список значений этого типа.

   Для двух данных файлов объединить загруженные списки в один список, упорядоченный по имени и сохранить
   результат в новый файл того же формата. Указание: всюду следует использовать монадический синтаксис
   (операции >>= и >>, функции ap, liftM и другие функции монадической обработки данных, использование
   блока do не допускается).
-}

import System.Environment
import Control.Monad
import Data.List

data Student = Student  
	{ name :: String, age :: Int, group :: Double } 
		deriving (Show,Ord,Eq)

{- создаем список студентов -}
studentList :: [String] -> [Student]
studentList [] = []
studentList xs = 
	(let [xName, xAge, xGroup ] = 
		take 3 xs in (Student xName (read xAge) (read xGroup)) ) : studentList (drop 3 xs )

{- читаем -}
readLines :: FilePath -> IO [Student]
readLines fileName = readFile fileName >>= return . studentList . lines

{- создаем список из двух -}
unionLists::IO[Student] -> IO[Student] -> IO[Student]
unionLists student1 student2 = (++) `liftM` student1 `ap` student2 >>= (return . sort)

{- > :main "stud.txt" "stud2.txt" вывод на консоль -}
main = getArgs >>= \[txt1,txt2] -> unionLists (readLines txt1) (readLines txt2)
