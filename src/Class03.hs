-- {-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-
ocamlのmoduleのような機能はないため、
型クラスを使って、DataFが持つべき関数を指定

TreeTをDataTのインスタンスにし、
TはGADTs拡張を使い抽象化した型にしている
-}
module Class03 where
  -- KindSignaturesで種注釈をつけている
  class DataF (f :: * -> *) where
    fmap :: (a -> b) -> f a -> f b
    occur :: f Bool -> Bool
    draw :: f String -> String

  -- GADTs拡張を用いて、data宣言に型注釈を与えている
  data T where
    Fix :: (DataF f) => f T -> T 
    Meta :: Int -> T

  -- 抽象的にした型T全般に適用できる関数
  occur' :: Int -> T -> Bool
  occur' k fix = case fix of
    Fix d -> occur (Class03.fmap (occur' k) d)
    Meta n -> n == k

  draw' :: T -> String
  draw' fix = case fix of
    Fix d -> draw (Class03.fmap draw' d)
    Meta n -> "{" ++ show n ++ "}"

  -- fold' :: DataF f => (f a -> a) -> (Int -> a) -> T -> a
  -- fold' phi psi fix = case fix of
  --   Fix d -> phi (Class03.fmap (fold' phi psi) d)
  --   Meta n -> psi n

  -- Tree module
  data TreeF a = Empty | Node (a, String, Int, a)
  instance DataF TreeF where
    fmap g tree = case tree of
      Empty -> Empty
      Node (left, s, n, right) -> Node (g left, s, n, g right)

    occur tree = case tree of
      Empty -> False
      Node (left, s, n, right) -> left || right

    draw tree = case tree of
      Empty -> "[]"
      Node (left, s, n, right) -> "[" ++ left ++ ", " ++ s ++ ", " ++ show n ++ ", " ++ right ++ "]"

    
  -- smart constructor
  empty :: T
  empty = Fix Empty

  node :: T -> String -> Int -> T -> T
  node l s n r = Fix (Node (l, s, n, r))

  meta :: Int -> T
  meta = Meta

  -- 例
  tree1 = empty
  tree2 = node empty "a" 3 (meta 0)
  tree3 = node tree2 "b" 4 (node empty "c" 1 empty)
  tree4 = node tree2 "b" 0 (node (meta 1) "c" 0 empty)

  -- 1
  data TypeF a = Int | Arrow (a, a) | Prod (a, a) deriving (Show)
  instance DataF TypeF where
    fmap g typ = case typ of
      Arrow (l, r) -> Arrow (g l, g r)
      Prod (l, r) -> Prod (g l, g r)
      Int -> Int

    occur typ = case typ of
      Int -> False
      Arrow (l, r) -> l || r
      Prod (l, r) -> l || r

    draw typ = case typ of
      Int -> "Int"
      Arrow (l, r) -> l ++ " -> " ++ r
      Prod (l, r) -> "(" ++ l ++ ", " ++ r ++ ")"

  -- smart constructor
  int :: T
  int = Fix Int
  arrow :: T -> T -> T
  arrow l r = Fix (Arrow (l, r))
  prod :: T -> T -> T
  prod l r = Fix (Prod (l, r))

  -- 例
  type1 = int
  type2 = arrow int (meta 0)
  type3 = prod int int
  type4 = arrow (prod (arrow int int) int) type3
  type5 = arrow (prod type2 int) (arrow type3 (meta 3))
