import Prelude hiding (foldl, foldr, map, concat, filter, reverse)

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f a [] = a
foldl f a (h : t) = foldl f (f a h) t

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f a [] = a
foldr f a (h : t) = f h (foldr f a t)

map :: (a -> b) -> [a] -> [b]  
map f list = foldr act [] list
	where act el res  = (f el) : res 

flatMap :: (a -> [b]) -> [a] -> [b] 
flatMap f list = foldl act [] list
	where act res el  = res ++ (f el)

concat :: [a] -> [a] -> [a]  
concat l1 l2 = foldr (:) l2 l1

filter :: (a -> Bool) -> [a] -> [a]  
filter f list = foldr act [] list
	where act el res = if (f el) then el : res else res

maxBy :: (a -> Integer) -> [a] -> a
maxBy f (h : t) = snd (foldl act ((f h), h) t)
	where act res el = if (f el) > (fst res) then ((f el), el) else res


minBy :: (a -> Integer) -> [a] -> a  
minBy f (h : t) = snd (foldl act ((f h), h) t)
	where act res el = if (f el) < (fst res) then ((f el), el) else res

reverse :: [a] -> [a]  
reverse list = foldl act [] list
	where act res el = el : res

elementAt :: Integer -> [a] -> a  
elementAt _ [] = error "List is empty"
elementAt index list | index < 0 = error "Index is out of bounds of list"
		     | index == 0 = head list
		     | otherwise = checkRes (foldl (\ lst el -> tail lst) list [1..index])
	where checkRes res = case res of 
		[] 	-> error "Index is out of bounds of list"
		(h : t) -> h 

indexOf :: String -> [String] -> Integer  
indexOf _ [] = -1
indexOf str list = foldr act (-1) (zip list [0..])
	where act (el, i) res = if str == el then i else res
