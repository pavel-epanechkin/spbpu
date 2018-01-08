newtype PSet a = PSet{ contains :: (a -> Bool) }
newtype PSet' a = PSet'{ contains' :: (a -> Bool) }
newtype PSet'' a = PSet''{ contains'' :: (a -> Bool) }


instance Monoid (PSet a) where
    mempty = PSet (\x -> False)
--Объединение двух множеств
    mappend (PSet a) (PSet b) = PSet (\x -> (a x) || (b x))

instance Monoid (PSet' a) where
    mempty = PSet' (\x -> True)
--Пересечение двух множеств
    mappend (PSet' a) (PSet' b) = PSet' (\x -> (a x) && (b x))

instance Monoid (PSet'' a) where
    mempty = PSet'' (\x -> False)
--Симметрическая разность двух множеств
    mappend (PSet'' a) (PSet'' b) = PSet'' (\x -> (a x) && not (b x) || not (a x) && (b x))

--Для остальных операций над множествами условие "mempty `mappend` x == x == x `mappend` mempty" не будет выполняться для всех x.
--Следовательно, для них не существует моноидов.


--Функция contains имеет тип (a -> Bool), отображение f, подающееся на вход fmap имеет тип (a -> b).
--Однако, функция contains не может оперировать типом b. Таким образом, f должно иметь тип (a -> a), что не удовлетворяет требованиям функтора.
--Да и само применение преобразования f является бессмысленным, так как описание вхождения элементов в новый PSet на основании описания 
--старого PSet не представляется возможным
--Поэтому самым адекватным является следующее описание функтора: 
instance Functor (PSet a) where
    fmap _ _ = PSet (\x -> False)
