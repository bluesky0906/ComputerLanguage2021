module Class01 where

-- question 1
map' :: (Int -> Int) -> [Int] -> [Int]
map' _ [] = []
map' f (x:xs) = f x:map' f xs

add1 :: [Int] -> [Int]
add1 = map' (+1)

double :: [Int] -> [Int]
double = map' (*2)

-- question 2
fold' :: (Int -> Int -> Int) -> Int -> [Int] -> Int
fold' _ acc [] = acc
fold' f acc (x:xs) = f x (fold' f acc xs)

prod :: [Int] -> Int
prod = fold' (*) 1

max' :: [Int] -> Int
max' = fold' (\x y -> if x > y then x else y) 0

-- question 3
data Tree a = Empty | Node (Tree a, a, Int, Tree a)

foldTree :: (a -> a -> Int -> a -> a) -> a -> Tree a -> a
foldTree _ acc Empty = acc
foldTree f acc (Node (left_tree, value1, value2, right_tree)) = f (foldTree f acc left_tree) value1 value2 (foldTree f acc right_tree)

sumProdTree :: Tree Int -> Int
sumProdTree = foldTree (\l v1 v2 r -> l + r + (v1 * v2)) 0

zeroString :: Tree [Char] -> [Char]
zeroString = foldTree (
  \l v1 v2 r -> case v2 of 0 -> l ++ v1 ++ r
                           _ -> l ++ r) ""