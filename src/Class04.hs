{-# LANGUAGE GADTs #-}

module Class04 where
  data CodeT =  U | I | Plus (CodeT, CodeT) | Times (CodeT, CodeT)

  data SumT a b = Left a | Right b

  data TypeF a where
    Unit :: () -> TypeF a
    Rec :: a -> TypeF a
    Sum :: SumT (TypeF a) (TypeF a) -> TypeF a
    Pair :: (TypeF a, TypeF a) -> TypeF a

  fmap :: CodeT -> (a -> b) -> TypeF a -> TypeF b
  fmap code f d = case (code, d) of
    (U,              Unit ())          -> Unit ()
    (I,              Rec d')         -> Rec (f d')
    (Plus (c1, c2),  Sum (Class04.Left d')) -> Sum (Class04.Left (Class04.fmap c1 f d'))
    (Plus (c1, c2),  Sum (Class04.Right d')) -> Sum (Class04.Right (Class04.fmap c2 f d'))
    (Times (c1, c2), Pair (d1, d2))    -> Pair (Class04.fmap c1 f d1, Class04.fmap c2 f d2)
    -- (_,              _)                -> failwith "Should not happen"
  newtype FixT = Fix (TypeF FixT)

  fold :: CodeT -> (TypeF a -> a) -> FixT -> a
  fold code phi (Fix d) =
    phi (Class04.fmap code (fold code phi) d)
  
  -- 自然数
  -- data f 'a = Zero | Succ of 'a
  nat :: CodeT
  nat = Plus (U, Times (U, I))

  -- constructor-like functions
  zero :: FixT
  zero = Fix (Sum (Class04.Left (Unit ())))

  suc :: FixT -> FixT
  suc n = Fix (Sum (Class04.Right (Pair (Unit (), Rec n))))

  ------------ 自然数の例 ------------
  one = suc zero
  two = suc one
  three = suc two

  lengthF :: TypeF Int -> Int
  lengthF d = case d of
    Sum (Class04.Left (Unit ())) -> 0
    Sum (Class04.Right (Pair (Unit (), Rec n))) -> 1 + n

  length :: FixT -> Int
  length = fold nat lengthF


  ------------ 木 ------------
  tree :: CodeT
  tree = Plus (U, Times (I, I))

  -- constructor-like functions
  empty = Fix (Sum (Class04.Left (Unit ())))
  node l r = Fix (Sum (Class04.Right (Pair (Rec l, Rec r))))

  -- 例
  tree1 = empty
  tree2 = node tree1 empty
  tree3 = node tree2 (node empty tree2)
  tree4 = node tree2 (node tree3 empty)

  countEmpty :: FixT -> Int
  countEmpty = fold tree phi where
    phi d = case d of
      Sum (Class04.Left (Unit ())) -> 1
      Sum (Class04.Right (Pair (Rec l, Rec r))) -> l + r

  size :: FixT -> Int
  size = fold tree phi where
    phi d = case d of
      Sum (Class04.Left (Unit ())) -> 0
      Sum (Class04.Right (Pair (Rec l, Rec r))) -> l + r + 1
  
