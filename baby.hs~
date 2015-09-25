doubleUs x y = doubleMe x + doubleMe y
doubleMe x = x + x
-- doubleUs x y = doubleMe x + doubleMe y
doubleSmallNumber x = if x > 100
			then x
			else doubleMe x
boomBang xs = [ if x `mod` 7 == 4 then "BOOM" else "tick.." | x<- xs, odd x] 
length' xs = sum [1 | _ <- xs]
removeNonUppercase st = [ c | c <- st, c `elem`['A'..'Z']]

