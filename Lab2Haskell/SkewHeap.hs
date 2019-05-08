module SkewHeap (SkewHeap (Leaf, Branch), merge, insert, extractMin, remove, isEmpty, removeVal) where 


import Test.QuickCheck    


data SkewHeap a = Leaf | Branch (SkewHeap a) a (SkewHeap a)


instance Ord a => Semigroup (SkewHeap a) where
    t <> t' = merge t t'

instance Ord a => Monoid (SkewHeap a) where
    mempty       = Leaf
    mappend t t' = t <> t'

instance Functor SkewHeap where
    fmap _ Leaf           = Leaf
    fmap f (Branch l n r) = Branch (fmap f l) (f n) (fmap f r)


instance (Ord a, Show a) => Show (SkewHeap a) where
    show Leaf = ""
    show heap = let (Just min) = extractMin heap
                in
                 show min ++ ", " ++ show (remove heap)   


merge :: Ord a => SkewHeap a -> SkewHeap a -> SkewHeap a
merge Leaf t                                = t
merge t Leaf                                = t
merge t@(Branch l n r) t'@(Branch l' n' r') | n <= n'   = Branch (merge t' r) n l
                                            | otherwise = Branch (merge t r') n' l'


insert :: Ord a => SkewHeap a -> a -> SkewHeap a
insert t a = t <> (Branch Leaf a Leaf)


extractMin :: Ord a => SkewHeap a -> Maybe a
extractMin Leaf             = Nothing
extractMin t@(Branch _ n _) = Just n


remove :: Ord a => SkewHeap a -> SkewHeap a
remove Leaf           = Leaf
remove (Branch l n r) = l <> r


removeVal :: Ord a => SkewHeap a -> a -> SkewHeap a
removeVal Leaf _ = Leaf
removeVal heap a = removeVal' [] heap a
    where
        removeVal' :: Ord a => [a] -> SkewHeap a -> a -> SkewHeap a
        removeVal' xs h@(Branch l n r) a | n == a    = (remove h) <> (fromList xs)
                                         | otherwise = removeVal' (n:xs) (remove h) a

fromList :: Ord a => [a] -> SkewHeap a
fromList [] = Leaf
fromList xs = fromList' xs Leaf
        where
            fromList' :: Ord a => [a] -> SkewHeap a -> SkewHeap a
            fromList' [] heap     = heap
            fromList' (x:xs) heap = fromList' xs (insert heap x)

toList :: Ord a => SkewHeap a -> [a]
toList Leaf = []
toList heap = toList' heap []
    where
        toList' :: Ord a => SkewHeap a -> [a] -> [a]
        toList' Leaf xs = xs
        toList' heap xs = toList' (remove heap) ((min):xs)
            where
                Just min = extractMin heap

isValid :: Ord a => SkewHeap a -> Bool
isValid heap = valid' $ toList heap
        where
            valid' :: Ord a => [a] -> Bool
            valid' []       = True
            valid' [x]      = True
            valid' (x:y:xs) = x >= y && valid' (y:xs)

isEmpty :: Ord a => SkewHeap a -> Bool
isEmpty Leaf = True
isEmpty _    = False


heap_prop :: [Int] -> [Int] -> Bool
heap_prop xs ys = length (toList $ (fromList xs) <> (fromList ys)) == length (xs ++ ys)