
-- test

{-
  Имеется тип данных для представления арифметических выражений с целыми числами
  и функция для вычисления его значения. 
-}

data Expr = I Int            -- целочисленная константа
            | Add Expr Expr   -- сумма двух выражений
            | Mul Expr Expr   -- произведение двух выражений

eval :: Expr -> Int
eval (I n) = n
eval (ex1 `Add` ex2) = eval ex1 + eval ex2
eval (ex1 `Mul` ex2) = eval ex1 * eval ex2

{-
  Реализуйте для этого типа экземпляр класса типов Eq так,
  чтобы равными считались любые два выражения, значения которых
  совпадают.
-}

instance Eq Expr where
  ex1 == ex2 = eval ex1 == eval ex2

{-
  Реализуйте для этого типа экземпляр класса типов Show так,
  чтобы выполнялись традиционные требования к записи арифметических
  выражений (и имеющиеся тесты). Для упрощения задания можно считать,
  что все числа в выражении положительные.
-}

data Operation = A | M

show1 (I n) _ = show n
show1 (ex1 `Add` ex2) A = (show1 ex1 A)++"+"++(show1 ex2 A)
show1 (ex1 `Add` ex2) M = "("++(show1 ex1 A)++"+"++(show1 ex2 A)++")"
show1 (ex1 `Mul` ex2) _ = (show1 ex1 M)++"*"++(show1 ex2 M)
		
instance Show Expr where
  show ex = show1 ex A

-- Тесты
test = all (== expr 4) exprs
       && all (/= (I 10)) exprs
       && map show exprs == ["(5+1)*7", "6+6*6", "12+17+13", "42"]
  where
    expr 1 = (I 5 `Add` I 1) `Mul` I 7
    expr 2 = I 6 `Add` (I 6`Mul` I 6)
    expr 3 = (I 12 `Add` I 17) `Add` I 13
    expr 4 = I 42

    exprs = map expr [1..4]

{-
  Напишите экземпляр класса типов Ord, который сравнивает выражения по их значению.
-}

instance Ord Expr where
  ex1 < ex2 = eval ex1 < eval ex2
  ex1 > ex2 = eval ex1 > eval ex2
  ex1 <= ex2 = ex1 < ex2 || ex1 == ex2
  ex1 >= ex2 = ex1 > ex2 || ex1 == ex2
  