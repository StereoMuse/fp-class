{-
   Тип Parser может быть определён следуюшим образом:
-}
import Control.Monad
newtype Parser a = Parser { apply :: String -> Maybe (a, String) }

{-
   Определите экземпляры классов Monad и MonadPlus для типа Parser в этом случае:
-}

instance Monad Parser where
  return x = Parser(\str -> Just (x, str))
  p >>= q = Parser(\str -> case (apply p str) of
	Just (x, strrr) -> apply (q x) strrr
	Nothing -> Nothing)
  fail _ = Parser (\str -> Nothing)

instance MonadPlus Parser where
  mzero = Parser (\str -> Nothing)
  p `mplus` q = Parser (\str -> case (apply p str) of 
	Nothing -> apply q str
	ps -> ps)