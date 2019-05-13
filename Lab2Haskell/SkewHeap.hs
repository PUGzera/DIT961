module SkewHeap (SkewHeap (Leaf, Branch), merge, insert, extractMin, remove, isEmpty, removeVal) where 


import Test.QuickCheck    
import Data.Maybe


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


removeVal :: Ord a => SkewHeap a -> a -> SkewHeap a
removeVal t a = removeVal' t Leaf a
                    where
                        removeVal' Leaf _ _ = Leaf
                        removeVal' t@(Branch l n r) t' a | n == a    = merge (merge l r) t'
                                                         | otherwise = removeVal' (remove t) (insert t' (fromJust $ extractMin t)) a  


extractMin :: Ord a => SkewHeap a -> Maybe a
extractMin Leaf             = Nothing
extractMin (Branch _ n _)   = Just n


remove :: Ord a => SkewHeap a -> SkewHeap a
remove Leaf           = Leaf
remove (Branch l n r) = l <> r


isEmpty :: Ord a => SkewHeap a -> Bool
isEmpty Leaf = True
isEmpty _    = False