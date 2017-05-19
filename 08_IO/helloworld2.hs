import Data.Char

main = do
	putStrLn "Hello, what's your name?"
	name <- getLine
	let bigName = map toUpper name
	putStrLn ("Hey " ++ bigName ++ "!")
