import Parser
import SimpleParsers
import Control.Applicative hiding (many, optional)
import Control.Monad

{-
   Определите тип данных, представляющий адрес URL следующего вида:

     <схема>://<логин>:<пароль>@<хост>:<порт>/<URL‐путь>?<параметры>#<якорь>

   Реализуйте соответствующий парсер, считая, что обязательными компонентами
   URL являются только схема с символами "://" и имя хоста, все остальные
   компоненты могут отсутствовать.
-}


data Scheme = FTP | HTTP | HTTPS | Unk String
              deriving Show
type Server = String
type Path = String
data URL = URL Scheme Login Password Server Port Path Parameters Anchor
           deriving Show

scheme = (string "https" >> return HTTPS) <|>
         (string "http" >> return HTTP) <|>
         (string "ftp" >> return FTP) <|>
         Unk `liftM` lowers

login = do
	log <- manyl (sat (/= ':'))
	char ':'
	return (log)

password = do
	pass <- many1 (sat (/= '@'))
	char @
	return pass
	
server = do
	serv <- manyl (sat (/= '@'))
	return serv

port = do
	p <- string ":"
	porttt <- manyl (sat (/= '?'))
	char '/'
	return porttt
	
path = do
	pathhh <- manyl (sat (/= '?'))
	char '?'
	return pathhh
	
params = do
	par <- manyl (sat (/= '#'))
	char '@'
	return par

url = URL <$>
      scheme <*>
      (string "://" >> optional "" login) <*>
	  optional "" pasword <*>
	  server <*>
	  optional "" port <*>
	  optional "" path <*>
	  optional "" params <*>
      many (sat $ const True)
