type Name = String
type Color = String

showInfo :: Name -> Color -> String
showInfo name color = "Name: " ++ name ++ ", Color: " ++ color

data Fruit = FruitConstr String
data Size = SizeConstr String

showFruit :: Fruit -> Size -> String
showFruit (FruitConstr f) (SizeConstr s) = "Fruit: " ++ f ++ ", Size: " ++ s

data Character = Elf { weapon :: String }
               | Hobbit { food :: String }
showCharacter :: Character -> String
showCharacter (Elf e) = "Elf: " ++ e
showCharacter (Hobbit h) = "Hobbit: " ++ h

data List a = Empty | Cons a (List a)
showList1 :: Show a => List a -> String
showList1 (Cons x xs) = (show x) ++ "," ++ showList1 xs
showList1 Empty = ""

data Tree a = Leaf | Node a (Tree a) (Tree a) deriving (Eq, Ord, Show)
treeFromList (x:xs) = Node x (treeFromList $ filter (<x) xs) (treeFromList $ filter (>x) xs)
treeFromList [] = Leaf

main =
  do let name = "Kenrick"::Name
         color = "red"::Color
         fruit = FruitConstr "Apple"
         size = SizeConstr "big"
         legolas = Elf { weapon="bow" }
         bilbo = Hobbit { food="pastries" }
         a = Cons 1 Empty
         ab = Cons 2 a
         abc = Cons 3 ab

     print $ showInfo name color
     print $ showFruit fruit size
     print $ showCharacter legolas
     print $ showCharacter bilbo
     print $ showList1 a
     print $ showList1 ab
     print $ showList1 abc
     print $ treeFromList [4,6,2,3,7,4,8,1]
