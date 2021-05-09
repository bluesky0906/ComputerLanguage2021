module Class02 where

-- 1
{-
先頭要素と、restを分けて定義
newtype と dataどちらも使える場合(ListT)は
制約が厳しいnewtypeを優先する
-}
data ListF a = Empty | Cons (Int, a)
newtype ListT = Fix (ListF ListT)

-- 2
empty :: ListT
empty = Fix Empty

cons :: Int -> ListT -> ListT
cons n a = Fix (Cons (n, a))

-- 3
list1 = empty
list2 = cons 2 empty
list3 = cons 3 (cons 2 empty)
list4 = cons (-3) (cons (-2) (cons 2 empty))

-- 4
{-
再帰するのはrest部分
-}
fmap' :: (t -> a) -> ListF t -> ListF a
fmap' f list = case list of
  Empty -> Empty
  Cons (n, a) -> Cons (n, f a)

-- 5
{-
(ListF a)型を受け取り、a型を返す関数を
fmap'を用いて再帰的に適用
--- イメージ ---
list = [1, 2]
phi (fmap' ((foldList phi) [1, 2]))
phi (fmap' (Cons (1, (foldList phi) [2])))
phi (fmap' (Cons (1, Cons (2, (foldList phi) Empty))))
-}
foldList :: (ListF a -> a) -> ListT -> a
foldList phi (Fix d) = phi (fmap' (foldList phi) d)

-- 6
sum1 :: ListF Int -> Int
sum1 list = case list of
  Empty -> 0
  Cons (n, a) -> n + a

sum' :: ListT -> Int
sum' = foldList sum1

-- 7
{-
Emptyの場合は最小定数を返す
-}
max1 :: ListF Int -> Int
max1 list = case list of
  Empty -> minBound :: Int
  Cons (n, a) -> if n > a then n else a

max' :: ListT -> Int
max' = foldList max1
