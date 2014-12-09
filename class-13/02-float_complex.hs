import Parser
import SimpleParsers
import ParseNumbers
import Data.Char
import Control.Applicative
{- Напишите парсер для вещественных чисел. -}
float :: Parser Float
float = floatR <|> floatN
	where
		floatR = do
			n <-integer
			char '.'
			m <- natural
			return $ read (show n ++ "." ++ show m)
		floatN = do
			n <-integer
			return (read (show n) :: Float)

{-
  Напишите парсер для представления комплексных чисел,
  записываемых в виде вещественной и мнимой части через запятую
  в круглых скобках, например, "(2.3, 1)".
  
-}
complex :: Parser (Float, Float)
complex = do
	string "("
	r <- float
	char ','
	m <- float
	string ")"
	return (r,m)

{-
  Напишите парсер для списка комплексных чисел (разделитель — точка с запятой),
  заключённого в квадратные скобки.
-}
complexList :: Parser [(Float, Float)]
complexList = do
	string "["
	flist []
		where flist ls = do
			(r,m) <- complex
			(char ';' >> flist (ls ++ [(r,m)])) <|> 
				(string "]" >> return (ls ++ [(r,m)]))

{-
  Модифицируйте предыдущий парсер таким образом, чтобы в исходной строке
  могли встречаться как комплексные числа, так и вещественные (мнимая часть
  при этом должна считаться равной нулю).
-}
complexList2 :: Parser [(Float, Float)]
complexList2 = undefined

{-
   Модифицируйте предыдущий парсер таким образом, чтобы компоненты списка
   разделялись запятой, а не точкой запятой. Постарайтесь реализовать
   требуемое с помощью вспомогательных парсеров, допускающих повторное применение.
-}
complexList3 :: Parser [(Float, Float)]
complexList3 = undefined
