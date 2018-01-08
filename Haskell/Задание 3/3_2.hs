data ReverseList a = RNil | RCons (ReverseList a) a

asList :: ReverseList a -> [a]
asList RNil = []
asList (RCons lst x) = (asList lst) ++ [x]

toList :: ReverseList a -> [a]
toList RNil = []
toList (RCons t h) = h : (toList t)

fromList :: [a] -> ReverseList a
fromList [] = RNil
fromList (h : t) = RCons (fromList t) h

--Eq, Ord, Show, Monoid and Functor
instance (Eq a) => Eq (ReverseList a) where
    (==) RNil RNil = True
    (==) RNil _ = False
    (==) _ RNil = False
    (==) (RCons lst x) (RCons lst' y) = x == y && lst == lst'

instance (Show a) => Show (ReverseList a) where
    show x = show (asList x)

instance (Ord a) => Ord (ReverseList a) where
    (<=) RNil _ = True
    (<=) _ RNil = False
    (<=) (RCons lst x) (RCons lst' y) = x <= y || lst <= lst'

instance Monoid (ReverseList a) where
    mempty = RNil
    mappend RNil y = y
    mappend x RNil = x
    mappend x (RCons lst el) = RCons (mappend x lst) el

instance Functor ReverseList where
    fmap f RNil = RNil
    fmap f (RCons lst el) = RCons (fmap f lst) (f el)
