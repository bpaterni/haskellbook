data Price = Price Integer deriving (Eq, Show)

data Manufacturer =
      Mini
    | Mazda
    | Tata 
      deriving (Eq, Show)

data Airline =
      PapuAir
    | CatpultsR'Us
    | TakeYourChancesUnited
      deriving (Eq, Show)

data PlaneSize = Small
               | Medium
               | Large
               deriving (Eq, Show)

data Vehicle = Car Manufacturer Price
             | Plane Airline PlaneSize
             deriving (Eq, Show)

myCar = Car Mini (Price 14000)

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _         = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _         = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

getManu :: Vehicle -> Manufacturer
getManu (Car m _) = m

