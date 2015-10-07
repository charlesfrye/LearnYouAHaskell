lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Better luck next time!"

sayMe :: (Integral a) => a -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe x = "Must be between 1 and 5"

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
	| otherwise = "Number is 0."

max' :: (Ord a) => a -> a -> a
max' a b
	| a > b = a
	| True  = b
