class Tofu t where
	tofu :: j a -> t a j
	--:k j :: * -> *
	--:k t :: * -> (* -> *) -> *

data Frank a b = Frank {frankField :: b a} deriving (Show)

instance Tofu Frank where
	tofu x = Frank x

data Barry t k p = Barry { yabba :: p, dabba :: t k}
	--:k Barry :: (* -> *) -> * -> * -> *

instance Functor (Barry a b) where
	fmap f (Barry {yabba = x, dabba = y}) =
		Barry {yabba = f x, dabba = y}
