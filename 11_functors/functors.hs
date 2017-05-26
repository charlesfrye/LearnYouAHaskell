import Data.Char
import Data.List

--instance Functor IO' where
--        fmap f action = do
--            result <- action
--           return (f result)

main = do line <- fmap (intersperse '-' . reverse . map toUpper) getLine
          putStrLn line

main' = do line <- getLine
           let line' = reverse line
           putStrLn $ "You said " ++ line' ++ " backwards!"

--main' = do line  <- (\xs -> intersperse '-' (reverse (map toUpper xs))) getLine
--     putStrLn line

--instance Functor ((->) r) where
--  fmap f g = (\x -> (f (g x)))

--class (Functor f) => Applicative' f where
--    pure :: a -> f a
--    (<*>) :: f (a -> b) -> f a -> f b

--instance Applicative' Maybe where
--    pure = Just
--    Nothing <*> _ = Nothing
--    (Just f) <*> something = fmap f something

myAction :: IO String
myAction = (++) <$> getLine <*> getLine

takeTwoPrint = do
        a <- myAction
        putStrLn $ "The two lines concatenated turn out to be: " ++ a
