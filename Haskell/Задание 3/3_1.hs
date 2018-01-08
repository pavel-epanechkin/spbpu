data WeirdPeanoNumber = Zero | Succ (WeirdPeanoNumber ) | Pred (WeirdPeanoNumber )

simplify :: WeirdPeanoNumber -> WeirdPeanoNumber
simplify Zero = Zero
simplify wpn = 
	let iter res num = case res of
		(Zero) 	 -> case num of
			(Zero)   -> res
			(Pred y) -> iter (Pred res) y 
			(Succ y) -> iter (Succ res) y
		(Succ x) -> case num of
			(Zero)   -> res
			(Pred y) -> iter x y
			(Succ y) -> iter (Succ res) y
		(Pred x) -> case num of
			(Zero)   -> res
			(Pred y) -> iter (Pred res) y 
			(Succ y) -> iter x y
	in
	iter Zero wpn

wpnToInt :: WeirdPeanoNumber -> Integer
wpnToInt x = case x of 
	Zero	 	-> 0
	(Succ num) 	-> 1 + wpnToInt num
	(Pred num) 	-> (wpnToInt num) - 1

intToWpn :: Integer -> WeirdPeanoNumber
intToWpn x | x == 0 = Zero
		   | x < 0 = Pred (intToWpn (x + 1))
		   | x > 0 = Succ (intToWpn (x - 1))
 	
instance Eq WeirdPeanoNumber where
	(==) x y = eq (simplify x) (simplify y) where 
		eq Zero Zero = True 
		eq _ Zero = False
		eq Zero _ = False
		eq (Succ a) (Succ b) = eq a b
		eq (Pred a) (Pred b) = eq a b
		eq _ _ = False

instance Ord WeirdPeanoNumber where
	(<=) x y = le (simplify x) (simplify y) where 
		le Zero Zero = True 
		le (Pred a) Zero = True
		le Zero (Succ b) = True
		le (Pred a) (Succ b) = True
		le (Succ a) (Succ b) = le a b
		le (Pred a) (Pred b) = le a b
		le _ _ = False

instance Show WeirdPeanoNumber where
    show x = show (wpnToInt x)

instance Num WeirdPeanoNumber where
	(+) x y = sum x y where 
		sum a Zero = a
		sum Zero b = b
		sum a (Pred num) = sum (Pred a) num
		sum a (Succ num) = sum (Succ a) num

	negate x = case x of 
		Zero 		-> Zero
		(Pred a) 	-> Succ (negate a)
		(Succ a)	-> Pred (negate a)

	(*) x y = mult (simplify x) (simplify y) where 
		mult Zero _ = Zero
		mult _ Zero = Zero
		mult x @ (Succ a) (Succ b) = x + (mult x b)
		mult x @ (Pred a) (Succ b) = x + (mult x b)
		mult x @ (Succ a) y @ (Pred b) = mult y x
		mult x @ (Pred a) y @ (Pred b) = mult (negate x) (negate y)
	
	signum x = sign (simplify x) where 
		sign Zero = Zero
		sign (Succ a) = Succ Zero
		sign (Pred a) = Pred Zero

	abs x = if signum x < Zero then negate x else x

	fromInteger x = intToWpn x

instance Enum WeirdPeanoNumber where
	toEnum = fromIntegral
	fromEnum = fromInteger.wpnToInt

instance Real WeirdPeanoNumber where
	toRational x = toRational $ wpnToInt x

instance Integral WeirdPeanoNumber where
	toInteger x = wpnToInt x

	quotRem Zero _ = (Zero, Zero)
	quotRem _ Zero = error "WeirdPeanoNum divided by zero"
	quotRem x y =
		let iter tmp @ (quot, rem) d = if rem >= d then iter (Succ (quot), rem - d) d else tmp in
		formRes x y (iter (Zero, abs $ simplify x) (abs $ simplify y)) 
		where formRes x y qr = if (signum x /= signum y) then (negate (fst qr), snd qr) else qr


-- Tests

testEq = (Succ(Succ(Succ(Succ(Pred(Pred(Zero)))))) == Succ(Succ(Zero))) && (2 /= Succ(Pred(Succ(Zero)))) 
--True

testOrd = ((Pred(Pred(Zero))) < Succ(Succ(Zero))) && (2 >= Succ(Pred(Succ(Zero))))
--True

testShow = show $ Succ(Pred(Succ(Zero)))
--"1"

testNum = (4 * x * x) - (signum (x + 2 * x + negate x)) where
	x = Succ(Succ(Succ(Succ(Pred(Succ(Succ(Zero)))))))
--99

testEnum = [(toEnum (5 - (fromEnum x)))..x] where
	x = Succ(Succ(Succ(Succ(Pred(Succ(Succ(Zero)))))))
--[0,1,2,3,4,5]

testReal = toRational (intToWpn 100)
--100 % 1

testIntegral = (toInteger $ fst (quotRem (intToWpn 100) (Succ(Succ(Succ(Zero)))))) * 3 + 1
--100
