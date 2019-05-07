module SkewHeap (SkewHeap (Leaf, Branch), merge, insert, extractMin, remove, isEmpty, removeVal) where 


data SkewHeap a = Leaf | Branch (SkewHeap a) a (SkewHeap a)

instance (Ord a, Show a) => Show (SkewHeap a) where
    show Leaf = ""
    show heap = (show (extractMin heap)) ++ ", " ++ (show (remove heap))

merge :: Ord a => SkewHeap a -> SkewHeap a -> SkewHeap a
merge Leaf t                                = t
merge t Leaf                                = t
merge t@(Branch l n r) t'@(Branch l' n' r') | n <= n'   = Branch (merge t' r) n l
                                            | otherwise = Branch (merge t r') n' l'


insert :: Ord a => SkewHeap a -> a -> SkewHeap a
insert t a = merge t (Branch Leaf a Leaf)


extractMin :: Ord a => SkewHeap a -> a
extractMin Leaf             = error "Cant extract min from empty heap"
extractMin t@(Branch _ n _) = n


remove :: Ord a => SkewHeap a -> SkewHeap a
remove Leaf           = Leaf
remove (Branch l n r) = merge l r


removeVal :: Ord a => SkewHeap a -> a -> SkewHeap a
removeVal Leaf _ = Leaf
removeVal heap a = removeVal' [] heap a

removeVal' :: Ord a => [a] -> SkewHeap a -> a -> SkewHeap a
removeVal' xs h@(Branch l n r) a | n == a    = merge (remove h) (listToSkewHeap xs Leaf)
                                 | otherwise = removeVal' (n:xs) (remove h) a

listToSkewHeap :: Ord a => [a] -> SkewHeap a -> SkewHeap a
listToSkewHeap [] heap = heap
listToSkewHeap (x:xs) heap = insert (listToSkewHeap xs heap) x

isEmpty :: Ord a => SkewHeap a -> Bool
isEmpty Leaf = True
isEmpty _    = False

