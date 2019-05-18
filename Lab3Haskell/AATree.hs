module AATree (Tree (Leaf, Branch), 
    emptyTree, 
    get,           
    insert,
    inorder,
    size,
    height,
    checkTree) where

import Data.List (sort)
import Test.QuickCheck

type Level = Int

data Tree a 
    = Leaf
    | Branch Level (Tree a) a (Tree a) deriving (Show, Eq)

level :: Tree a -> Int
level Leaf = 0
level (Branch i _ _ _) = i    


emptyTree :: Tree a
emptyTree = Leaf


insert :: Ord a => a -> Tree a -> Tree a
insert a Leaf = Branch 1 Leaf a Leaf
insert a (Branch i l n r)
    | a >= n    = fix $ Branch i (insert a l) n r
    | otherwise = fix $ Branch i l n (insert a r)
  

fix :: Ord a => Tree a -> Tree a
fix = split . skew


skew :: Ord a => Tree a -> Tree a
skew Leaf = Leaf
skew t@(Branch i (Branch i' l1 n1 r1) n r) 
    | i' == i = Branch i' l1 n1 (Branch i r1 n r)
    | otherwise = t
skew t = t


split :: Ord a => Tree a -> Tree a
split Leaf = Leaf
split t@(Branch i l n (Branch i' l1 n1 r1@(Branch irr _ _ _)))
    | i == irr = Branch (i' + 1) (Branch i l n l1) n1 r1
    | otherwise = t
split t = t


fromList :: Ord a => [a] -> Tree a
fromList [] = Leaf
fromList xs = fromList' xs Leaf
    where
        fromList' [] t     = t
        fromList' (x:xs) t = fromList' xs (insert x t)  


inorder :: Tree a -> [a]
inorder Leaf = []
inorder (Branch i l n r) = inorder r ++ n : inorder l


height :: Tree a -> Int
height Leaf = 0
height (Branch i l n r) = 1 + height r


get :: Ord a => a -> Tree a -> Maybe a
get _ Leaf = Nothing
get a (Branch _ l n r) 
        | a == n = Just n
        | a > n  = get a r
        | otherwise = get a l 


size :: Tree a -> Int
size = length . inorder


checkTree :: Ord a => Tree a -> Bool
checkTree root = isSorted (inorder root) &&
                 all checkLevels (nodes root)
                 where
                    nodes x 
                     | isEmpty x = []
                     | otherwise = x:nodes (leftSub x) ++ nodes (rightSub x)


isSorted :: Ord a => [a] -> Bool
isSorted xs = xs == sort xs


checkLevels :: Tree a -> Bool
checkLevels Leaf = True
checkLevels (Branch i l _ r) = 
    i > level l &&
    i >= level r && 
    i > (level $ rightSub r)


isEmpty :: Tree a -> Bool
isEmpty Leaf = True
isEmpty _    = False


leftSub :: Tree a -> Tree a
leftSub Leaf = Leaf
leftSub (Branch _ l _ _) = l


rightSub :: Tree a -> Tree a
rightSub Leaf = Leaf
rightSub (Branch _ _ _ r) = r


inorder_prop :: Ord a => [a] -> Bool
inorder_prop = isSorted . inorder . fromList


tree_prop :: Ord a => [a] -> Bool
tree_prop = checkTree . fromList