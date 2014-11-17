import Control.Monad
import Data.List
import System.Environment
import Data.Maybe

{-
   Дан текстовый файл (inventory.txt)  с перечислением всей имеющейся на складе
   лёгкой брони. Сформируйте список имеющихся полных комплектов брони одного
   вида (kind). Указание: в решении рекомендуется пользоваться монадическими
   операциями всюду, где только возможно.
-}

data ArmorType = Shield | Helmet | Gauntlets | Boots | Cuirass
   deriving (Show, Read, Eq, Ord, Enum, Bounded)
data ArmorKind = Chitin | Hide | Leather | Elven | Scaled | Glass | ImperialLight
   deriving (Show, Read, Eq, Ord, Enum, Bounded)
data ArmorItem = ArmorItem ArmorKind ArmorType 
   deriving (Show, Eq)
data ArmorKit = ArmorKit ArmorKind [ArmorType]
   deriving (Show, Eq)

parserStr s = 
	let (kd, tp) = span (/= ' ') s in ArmorItem (read kd) (read tp)
   
loadInventory :: FilePath -> IO [ArmorItem]
loadInventory fileName = (readFile fileName) >>= return . map parserStr . lines

buildArmorKit :: ArmorKind -> [ArmorItem] -> Maybe ArmorKit
buildArmorKit kd xs = if (length $ nub tp) == 5 then Just (ArmorKit kd tp) else Nothing
	where
		tp = map (\(ArmorItem _ t) -> t) $ filter (\(ArmorItem k _) -> k == kd) xs

buildKits :: [ArmorItem] -> Maybe [ArmorKit]
buildKits xs = sequence $ filter (isJust) ls
	where
		ls = zipWith buildArmorKit [Chitin, Hide, Leather, Elven, Scaled, Glass, ImperialLight] (replicate 7 xs)

{- :main "inventory.txt" -}
main = (head `liftM` getArgs) >>= loadInventory >>= print . buildKits
