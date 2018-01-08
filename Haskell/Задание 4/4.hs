data FunMonad a = FunMonad{ fun :: () -> a }

instance Functor FunMonad where
    fmap f x = FunMonad ( \() -> f ( (fun x) () ) )

instance Applicative FunMonad where
	pure x = FunMonad ( \() -> x )
	FunMonad a <*> FunMonad b = FunMonad ( \() -> (a ()) (b ()) )

instance Monad FunMonad where
	return x = FunMonad ( \() -> x )
	x >>= f = f ( (fun x) () )

instance (Show a) => Show (FunMonad a) where
	show x = "FunMonad " ++ ( show ( (fun x) () ) )

--Test
fmonad = FunMonad ( \() -> 10 )
-- FunMonad 10
fmonad' = fmap (*10) fmonad
-- FunMonad 100
fmonad'' = fmonad >>= (\x -> FunMonad ( \() -> [1..x] ) )
-- FunMonad [1,2,3,4,5,6,7,8,9,10]