{-# LANGUAGE GADTs #-}

module Class06 where
  data CodeT =  U | I Int | Plus (CodeT, CodeT) | Times (CodeT, CodeT)

  data SumT a b = Left a | Right b

  data TypeF a where
    Unit :: () -> TypeF a
    Rec :: a -> TypeF a
    Sum :: SumT (TypeF a) (TypeF a) -> TypeF a
    Pair :: (TypeF a, TypeF a) -> TypeF a

  fmap :: CodeT -> (Int -> a -> b) -> TypeF a -> TypeF b
  fmap code f d = case (code, d) of
    (U,              Unit ())          -> Unit ()
    (I n,              Rec d')         -> Rec (f n d')
    (Plus (c1, c2),  Sum (Class06.Left d')) -> Sum (Class06.Left (Class06.fmap c1 f d'))
    (Plus (c1, c2),  Sum (Class06.Right d')) -> Sum (Class06.Right (Class06.fmap c2 f d'))
    (Times (c1, c2), Pair (d1, d2))    -> Pair (Class06.fmap c1 f d1, Class06.fmap c2 f d2)

  newtype FixT = Fix (TypeF FixT)

  fold :: CodeT -> (Int -> TypeF a -> a) -> Int -> FixT -> a
  fold code phi n (Fix d) =
    phi n (Class06.fmap code (fold code phi) d)

  -- ZigZagデータ -------------------------------------------------------------------------
  zigF :: CodeT
  zigF = Plus (I 1, U)

  zagF :: CodeT
  zagF = I 0

  zigzagF :: CodeT
  zigzagF = Plus (zigF, zagF)

  -- constructor-like functions 
  endo :: FixT
  endo = Fix (Sum (Class06.Left (Sum (Class06.Right (Unit ())))))

  zig :: FixT -> FixT
  zig d1 = Fix (Sum (Class06.Left (Sum (Class06.Left (Rec d1)))))

  zag :: FixT -> FixT
  zag d0 = Fix (Sum (Class06.Right (Rec d0)))

  -- データの例 
  zagEnd = zag endo
  zigZagEnd = zig (zag endo)
  zagZigZagEnd = zag (zig (zag endo))
  zigZagZigZagEnd = zig (zag (zig (zag endo)))

  length0F :: TypeF Int -> Int
  length0F d = case d of
    Sum (Class06.Right (Unit ())) -> 0
    Sum (Class06.Left (Rec d1)) -> 1 + d1

  length1F :: TypeF Int -> Int
  length1F d = case d of
    Rec d0 -> 1 + d0

  lengthF :: Int -> TypeF Int -> Int
  lengthF n d = case (n, d) of
    (0, Sum (Class06.Left d))  -> length0F d
    (1, Sum (Class06.Right d)) -> length1F d

  length :: Int -> FixT -> Int
  length = fold zigzagF lengthF


  -- 課題 -------------------------------------------------------------------------
  valueF :: CodeT
  valueF = Plus (U, Times(U, I 1))
  exprF :: CodeT
  exprF = Plus (I 0, Times(I 1, I 1))
  lambdaF :: CodeT 
  lambdaF = Plus (valueF, exprF)

  countVar0F :: TypeF Int -> Int
  countVar0F d = case d of
    Sum (Class06.Left (Unit ())) -> 1
    Sum (Class06.Right (Pair (Unit (), Rec d1))) -> d1

  countVar1F :: TypeF Int -> Int
  countVar1F d = case d of
    Sum (Class06.Left (Rec d0)) -> d0
    Sum (Class06.Right (Pair (Rec d1a, Rec d1b))) -> d1a + d1b

  countVarF :: Int -> TypeF Int -> Int
  countVarF n d = case (n, d) of
    (0, Sum (Class06.Left d))  -> countVar0F d
    (1, Sum (Class06.Right d)) -> countVar1F d

  countVar :: Int -> FixT -> Int
  countVar = fold lambdaF countVarF
  
  -- constructor-like functions 
  var = Fix (Sum (Class06.Left (Sum (Class06.Left (Unit ())))))
  fun d1 = Fix (Sum (Class06.Left (Sum (Class06.Right (Pair (Unit (), Rec d1))))))
  val d0 = Fix (Sum (Class06.Right (Sum (Class06.Left (Rec d0)))))
  app d1a d1b = Fix (Sum (Class06.Right (Sum (Class06.Right (Pair (Rec d1a, Rec d1b))))))

  -- 例
  lambda1 = var
  lambda2 = val lambda1
  lambda3 = fun lambda2
  lambda4 = val lambda3
  lambda5 = app lambda2 lambda4
  