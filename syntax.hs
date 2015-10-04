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
