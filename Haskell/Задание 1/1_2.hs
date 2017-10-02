--Синус угла (в радианах) на основе ряда Маклорена. 
sinus :: Double -> Double
sinus x =
	let iter row n x = if ((row !! 0) == 0) then row else iter ((-1 ** n) * (row !! 0) * x**2 / ((2 * n)*(2 * n + 1)) : row) (n + 1) x in
	sum (iter [x] 1 x)

--НОД двух целых чисел
nod :: Integer -> Integer -> Integer
nod a b | (a < 0 || b < 0) = nod (abs a) (abs b)
		| a < b = nod b a
		| a >= b = if (a `mod` b == 0) then b else nod b (a `mod` b)

--Возведение целого числа в целую степень
pow :: Integer -> Integer -> Double
pow m n = 
	let powNat m n | n == 0 = 1
				   | m == 0 = 0
				   | n `mod` 2 == 1 = m * powNat m (n - 1)
				   | n `mod` 2 == 0 = powNat (m * m) (n `div` 2) in
    if n >= 0 then toDouble (powNat m n) else  1 / toDouble (powNat m (-1 * n))
	where toDouble x = fromIntegral x::Double

--Возведение целого числа в натуральную степень
powN :: Integer -> Integer -> Integer
powN m n | n == 0 = 1
		 | m == 0 = 0
		 | n `mod` 2 == 1 = m * powN m (n - 1)
		 | n `mod` 2 == 0 = powN (m * m) (n `div` 2)
