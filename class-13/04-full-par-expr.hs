import Parser
import ParseNumbers
import SimpleParsers
import Control.Applicative hiding (many, optional)
import Control.Monad

data Expr = Con Int | Bin Op Expr Expr
  deriving (Show, Eq)
data Op = Plus | Minus
  deriving (Show, Eq)

{-
   Модифицируйте эту грамматику и парсер таким образом, чтобы они корректно
   распознавали заключённые в скобки числовые литералы.
-}

{-
expr  ::=  exprrr | '(' exprrr ')'
op    ::=  '+' | '-'
nat   ::=  {digit}+
digit ::=  '0' | '1' | '2' | ... | '9'
-}

expr :: Parser Expr
expr = bracket "(" ")" exprrr <|> exprrr

exprrr = token term >>= elseExpr
  where
	elseExpr e1 = optional e1 $ do
    p <- token op
    e2 <- token term
		where term = constant <|> bracket "(" ")" constant <|>
			(token (char '(') >> constant <|> 
			(do
				x <- constant
				token (char ')')
				return x)
	elseExpr $ Bin p e1 e2
	
constant = Con `liftM` natural

op = (symbol "+" >> return Plus) <|> (symbol "-" >> return Minus)

test =  parse expr "2" == parse expr "(2)" &&
        parse expr "((2)+3)" == parse expr "(2+3)" 
