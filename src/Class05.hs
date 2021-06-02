{-# LANGUAGE GADTs #-}

module Class05 where
  data CodeT =  U | I | P | Plus (CodeT, CodeT) | Times (CodeT, CodeT) deriving(Show)

  data SumT a b = Left a | Right b deriving(Show)

  data TypeF a p where 
    Unit :: () -> TypeF a p
    Rec :: a -> TypeF a p
    Par :: p -> TypeF a p
    Sum :: SumT (TypeF a p) (TypeF a p) -> TypeF a p
    Pair :: (TypeF a p, TypeF a p) -> TypeF a p
    deriving(Show)

  fmap :: CodeT -> (a -> b) -> TypeF a p -> TypeF b p
  fmap code f d = case (code, d) of
    (U,              Unit ())          -> Unit ()
    (I,              Rec d')         -> Rec (f d')
    (P,              Par v)          -> Par v
    (Plus (c1, c2),  Sum (Class05.Left d')) -> Sum (Class05.Left (Class05.fmap c1 f d'))
    (Plus (c1, c2),  Sum (Class05.Right d')) -> Sum (Class05.Right (Class05.fmap c2 f d'))
    (Times (c1, c2), Pair (d1, d2))    -> Pair (Class05.fmap c1 f d1, Class05.fmap c2 f d2)

  newtype FixT p = Fix (TypeF (FixT p) p) deriving(Show)

  fold :: CodeT -> (TypeF a p -> a) -> FixT p -> a
  fold code phi (Fix d) =
    phi (Class05.fmap code (fold code phi) d)


  -- 自然数　-----------------------------------------------------------
  nat :: CodeT
  nat = Plus (U, Times (U, I))

  -- constructor-like functions
  zero :: FixT p
  zero = Fix (Sum (Class05.Left (Unit ())))

  suc :: FixT p -> FixT p
  suc n = Fix (Sum (Class05.Right (Pair (Unit (), Rec n))))

  -- 自然数の例
  one = suc zero
  two = suc one
  three = suc two

  length :: FixT p -> Int
  length = fold nat lengthF
    where
      lengthF d = case d of
        Sum (Class05.Left (Unit ())) -> 0
        Sum (Class05.Right (Pair (Unit (), Rec n))) -> 1 + n

  -- 自然数のリスト　-----------------------------------------------------------
  list :: CodeT
  list = Plus (U, Times (P, I))

  -- constructor-like functions
  empty :: FixT q
  empty = Fix (Sum (Class05.Left (Unit ())))

  cons :: q -> FixT q -> FixT q
  cons n l = Fix (Sum (Class05.Right (Pair (Par n, Rec l))))

  -- 自然数のリストの例
  lst1 = cons one empty
  lst2 = cons one (cons two empty)
  lst3 = cons zero (cons one (cons two (cons three empty)))

  lengthList :: FixT p -> Int
  lengthList = fold list lengthF
    where
      lengthF :: TypeF Int q -> Int  
      lengthF d = case d of
        Sum (Class05.Left (Unit ())) -> 0
        Sum (Class05.Right (Pair (Par n, Rec l))) -> 1 + l

  sumList :: FixT (FixT p) -> Int
  sumList = fold list sumF
    where
      sumF :: TypeF Int (FixT q) -> Int
      sumF d = case d of
        Sum (Class05.Left (Unit ())) -> 0
        Sum (Class05.Right (Pair (Par n, Rec l))) -> Class05.length n + l

  -- 木 (ここから課題) -----------------------------------------------------------
  tree :: CodeT
  tree = Plus (U, Times (I, Times (P, I)))

  -- constructor-like functions
  emptyTree :: FixT p
  emptyTree = Fix (Sum (Class05.Left (Unit ())))

  node :: FixT p -> p -> FixT p -> FixT p
  node l n r = Fix (Sum (Class05.Right (Pair (Rec l, Pair (Par n, Rec r)))))

  -- 例
  -- FixT (FixT nat)型
  tree1 = emptyTree
  tree2 = node emptyTree one emptyTree
  tree3 = node tree2 two (node emptyTree zero tree2)
  tree4 = node tree3 three tree2

  -- FixT (FixT list)型
  tree5 = node emptyTree lst1 emptyTree
  tree6 = node tree5 lst2 (node emptyTree lst1 tree5)
  tree7 = node tree6 lst3 tree5

  -- 問4
  countEmpty :: FixT (FixT p) -> Int
  countEmpty = fold tree countF
    where
      countF :: TypeF Int (FixT p) -> Int
      countF d = case d of
        Sum (Class05.Left (Unit ())) -> 1
        Sum (Class05.Right (Pair (Rec l, Pair (Par n, Rec r)))) -> l + r

  -- 問5
  size :: FixT p -> Int
  size = fold tree sizeF
    where
      sizeF d = case d of
        Sum (Class05.Left (Unit ())) -> 0
        Sum (Class05.Right (Pair (Rec l, Pair (Par n, Rec r)))) -> l + r + 1

  -- 問6
  sumTree :: FixT (FixT p) -> Int
  sumTree = fold tree sumF
    where
      sumF :: TypeF Int (FixT p) -> Int
      sumF d = case d of
        Sum (Class05.Left (Unit ())) -> 0
        Sum (Class05.Right (Pair (Rec l, Pair (Par n, Rec r)))) -> l + r + Class05.length n

  -- 問7
  sumListTree :: FixT (FixT (FixT p)) -> Int
  sumListTree = fold tree sumF
    where
      sumF :: TypeF Int (FixT (FixT p)) -> Int
      sumF d = case d of
        Sum (Class05.Left (Unit ())) -> 0
        Sum (Class05.Right (Pair (Rec l, Pair (Par n, Rec r)))) -> l + r + sumList n
