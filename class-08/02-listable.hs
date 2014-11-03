{-# LANGUAGE TypeSynonymInstances,FlexibleInstances #-}
{-
   Определите класс типов Listable с двумя функциями:
   toList :: a -> [a]
   fromList :: [a] -> a
   
  Объявите экземпляры класса типов Listable для следующих типов:
  1) String - строка разбивается по пробелам на список слов.
  2) Integer - любое целое число разбивается на список цифр.

  -- testSf
  -- testSt
  -- testIf
  -- testIt
-}

class Listable a
	where
		toList :: a -> [a]
		fromList :: [a] -> a
		
instance Listable String 
	where
		--toList :: a -> [a]
		toList = words
		
		--fromList :: [a] -> a
		fromList = unwords

instance Listable Integer 
	where
		--toList :: a -> [a]
		toList b = reverse $ modDiv b
			where
				modDiv 0 = []
				modDiv a = mod a 10 : (modDiv $ div a 10)
		
		--fromList :: [a] -> a
		fromList = fst . foldr (\ n (s, m) -> (n*m + s, m*10)) (0, 1)

-- Тесты
testSf = fromList ["seven", "seven", "two"] == "seven seven two"
testSt = toList "one two seven" == ["one", "two", "seven"]

testIf = fromList ([7,7,7]::[Integer]) == 777
testIt = toList 777 == ([7,7,7]::[Integer])
