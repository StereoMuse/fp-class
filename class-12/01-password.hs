{-
   Модифицируйте представленное на лекции решение задачи о запросе пароля,
   удовлетворяющего требованиям по стойкости, следующим образом:
   - в командной строке задаются ограничения (минимальная длина, наличие букв,
     наличие цифр, наличие знаков пунктуации);
   - к стеку монад добавляется монада Reader, и с её помощью организуется
     доступ к ограничениям в функции isValid;
   - все попытки ввода пароля протоколируются средствами монады Writer.
-}

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Writer
import Control.Monad.Reader
import Data.Char
import Data.Maybe
import System.Environment
				
--isValid :: String -> [String] -> Bool
isValid s allS =( length s >= read minLength &&(not (read alpha) || (any isAlpha s) )&&(not (read number) || (any isNumber s) )&&(not (read punctuation) || (any isPunctuation s) ) )
	where
		[minLength, alpha, number,punctuation] = allS

--getValidPassword :: MaybeT IO String
getValidPassword sss = do
	liftIO $ putStrLn "Введите новый пароль:"
	s <- liftIO getLine
	tell $ [s]
	guard (isValid s sss)
	return s
 
--askPassword :: MaybeT IO ()
askPassword = do
	con <- lift ask
	value <- msum $ repeat ( getValidPassword con )
	liftIO $ putStrLn "Сохранение в базе данных..."

main = do
	allS <- getArgs
	(x,passwords) <- runWriterT (runReaderT (runMaybeT askPassword) allS )
	print passwords
