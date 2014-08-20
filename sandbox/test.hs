type Name = String
type Color = String

showInfo :: Name => Color => String
showInfo name color = "Name: " ++ name ++ ", Color: " ++ color

data Fruit = FruitConstr String
data Size = SizeConstr String

showFruit :: Fruit => Size => String
showFruit (FruitConstr f) (SizeConstr s) = "Fruit: " ++ f ++ ", Size: " ++ s

data Character = Elf { weapon :: String }
               | Hobbit { food :: String }
showCharacter :: Character => String
showCharacter (Elf e) = "Elf: " ++ e
showCharacter (Hobbit h) = "Hobbit: " ++ h

main =
  do let name = "Kenrick"::Name
         color = "red"::Color
         fruit = FruitConstr "Apple"
         size = SizeConstr "big"
         legolas = Elf { weapon="bow" }
         bilbo = Hobbit { food="pastries" }

     print $ showInfo name color
     print $ showFruit fruit size
     print $ showCharacter legolas
     print $ showCharacter bilbo
