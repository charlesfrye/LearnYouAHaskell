data Person' = Person' String String Int Float String String deriving (Show)

data Person = Person { firstName :: String
    , lastName :: String
    , age :: Int
    , height :: Float
    , phoneNumber :: String
    , flavor :: String
    } deriving (Show)

data Car' = Car' String String Int deriving (Show)

data Car = Car {company :: String, model :: String,
        year :: Int} deriving (Show)

tellCar :: Car -> String
tellCar (Car {company = c, model = m, year =y}) =
    "This " ++ c ++ " " ++ m ++ " was made in " ++ show y

data Vector a  = Vector a a a deriving (Show)

vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vplus` (Vector l m n) =
        Vector (i+l) (j+m) (k+n)

vectMult :: (Num t) => Vector t -> t -> Vector t
(Vector i j k) `vectMult` m = Vector (i*m) (j*m) (k*m)

scalarMult :: (Num t) => Vector t -> Vector t -> t
(Vector i j k) `scalarMult` (Vector l m n) =
        i*l + j*m + k*n

data Person'' = Person'' { first :: String
            , last :: String
            , age' :: Int
            } deriving (Eq, Show, Read)

data Day = Monday | Tuesday | Wednesday | Thursday | Friday
        | Saturday | Sunday
    deriving  (Eq, Ord, Show, Read, Bounded, Enum)

type PhoneNumber = String
type Name = String
type PhoneBook = [(Name,PhoneNumber)]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnumber pbook = (name,pnumber) `elem` pbook

--firstName :: Person' -> String
--firstName (Person' firstname _ _ _ _ _) =  firstname
--
--lastName :: Person' -> String
--lastName = (Person' _ lastname _ _ _ _) = lastname
--
--age :: Person' -> Int
--age (Person' _ _ age _ _ _) = age
--
--height :: Person' -> Float
--height (Person' _ _ _ height _ _) = height
--
--phoneNumber :: Person' -> String
--phoneNumber (Person' _ _ _ _ number _) = number
--
--flavor :: Person' -> String
--flavor (Person' _ _ _ _ _ flavor) = flavor
