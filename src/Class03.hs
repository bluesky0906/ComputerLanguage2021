-- {-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Class03 where
  -- KindSignaturesで種注釈をつけている
  class DataF (f :: * -> *) where
    fmap :: (a -> b) -> f a -> f b
    occur :: f Bool -> Bool

  -- GADTs拡張を用いて、data宣言に型注釈を与えている
  data T where
    Fix :: (DataF t) => t T -> T 
    Meta :: Int -> T

  occur' :: Int -> T -> Bool
  occur' k fix = case fix of
    Fix d -> occur (Class03.fmap (occur' k) d)
    Meta n -> n == k
    
  data TreeF a = Empty | Node (a, String, Int, a)
  instance DataF TreeF where
    fmap g tree = case tree of
      Empty -> Empty
      Node (left, s, n, right) -> Node (g left, s, n, g right)

    occur tree = case tree of
      Empty -> False
      Node (left, s, n, right) -> left || right

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