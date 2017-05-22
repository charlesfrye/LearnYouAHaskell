import Data.Char
import Data.List

main = do line <- fmap (intersperse '-' . reverse . map toUpper) getLine
	  putStrLn line

--main' = do line  <- (\xs -> intersperse '-' (reverse (map toUpper xs))) getLine
--	   putStrLn line

instance Functor ((->) r) where
	fmap f g = (\x -> (f (g x)))

class (Functor f) => Applicative f where
	pure :: a -> f a
	(<*>) :: f (a -> b) -> f a -> f b

instance Applicative Maybe where
	pure = Just
	Nothing <*> _ = Nothing
	(Just f) <*> something = fmap f something
