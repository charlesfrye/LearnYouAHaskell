lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Better luck next time!"

sayMe :: (Integral a) => a -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe x = "Must be between 1 and 3"

factorial :: (Integral a) => a ->a
factorial 0 = 1
factorial n = n * factorial(n-1)

charName :: Char -> String
charName 'a' = "Albert"
charName 'b' = "Broseph"
charName 'c' = "Cecil"

add2DVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
add2DVectors (x1,y1) (x2,y2) = (x1 + x2, y1+y2)

--let xs = [(1,3), (4,3), (2,4), (5,3), (5,6), (3,1)]

head' :: [a] -> a
head' [] = error "empty list"
head' (x:_) = x

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "The list is long. Its first two elements are: " ++ show x ++ " and " ++ show y

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:tail) = 1 + length' tail

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (head:tail) = head + sum' tail

capital :: String -> String
capital "" = "Empty string, whoops!"
capital all@(first:_) = "The first letter of " ++ all ++ " is " ++ [first]

guardTell :: (RealFloat a) => a -> String
guardTell val
    | val < 0  = "Number is negative."
    | val > 0  = "Number is positive."
--  | otherwise = "Number is 0."
guardTell 0 = "Number is 0."

max' :: (Ord a) => a -> a -> a
max' a b
    | a > b = a
    | True  = b

max'' :: (Ord a) => a -> a -> a
max'' a b | a > b = a | otherwise = b

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | bmi <= under = "underweight"
    | bmi <= healthy = "healthy weight"
    | bmi <= over = "overweight"
    | otherwise = "obese"
    where bmi = weight / height ^ 2
          (under, healthy, over) = (18.5, 25.0, 30.0)

calcBmis :: (RealFloat a) => [(a,a)] -> [a]
calcBmis lst = [bmi w h | (w,h) <- lst]
    where bmi weight height = weight/ height ^ 2

calcBmis' :: (RealFloat a) => [(a,a)] -> [a]
calcBmis' lst = [w / h^2 | (w,h) <- lst]
--  where bmi weight height = weight/ height ^ 2

calcBmis'' :: (RealFloat a) => [(a,a)] -> [a]
calcBmis'' xs = [bmi | (w,h) <- xs, let bmi = w / h ^ 2]

--calcBmisOver :: (RealFloat a) => [(a,a)] -> [a]
--calcBmisOver xs = [bmi | (w,h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0]

--cylinder :: (RealFloat a) => a -> a -> a
--cylinder r h =
--  let sideArea = 2 * pi * r * h
--      topArea = pi * r ^2
--  in sideArea + 2 * topArea

head'' :: [a] -> a
head'' xs = case xs of [] -> error "No head for empty lists!"
                       (x:_) -> x

describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of [] -> "empty."
                                               [x] -> "a singleton list"
                                               xs -> "a longer list"
