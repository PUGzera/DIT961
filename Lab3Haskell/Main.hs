import AATree
import Data.List (nub)

main :: IO()
main = do
    content <- getContents
    let ws = words content
    let tree = foldr insert Leaf ws
    let oh = (ceiling $ logBase 2 $ fromIntegral $ length ws + 1) - 1
    let h = height tree
    print $ "Size: " ++ (show $ size tree)
    print $ "Height: " ++ show h
    print $ "Optimal Height: " ++ show oh
    print $ "Height / Optimal Height: " ++ show (fromIntegral h / fromIntegral oh)
    print $ "checkTree: " ++ (show $ checkTree tree)
    print $ "First 20 words: " ++ (unwords $ take 20 $ inorder tree)