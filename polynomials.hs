type Poly = [Double]

expand :: Poly -> Int -> Poly
expand l n = replicate (n - (length l)) 0 ++ l

remove_0s :: Poly -> Poly
remove_0s (0:xs) = remove_0s xs
remove_0s l = l

less :: Poly -> Poly -> Bool
less _ [] = False
less [] _ = True
less ps qs = p < q || p == q && helper ps qs
  where
    p = (length ps)
    q = (length qs)
    helper _ [] = False
    helper [] _ = True
    helper (x:xs) (y:ys) = if x < y then True else if x > y then False else helper xs ys

pneval :: Poly -> Double -> Double
pneval l v = helper l v 0
  where
    helper [] _ res = res
    helper (x:xs) a res = helper xs a (res * a + x)


pnadd :: Poly -> Poly -> Poly
pnadd ps qs = remove_0s (if p < q then zipWith (+) (expand ps q) qs else zipWith (+) ps (expand qs p))
  where
    p = length ps
    q = length qs

pnmul :: Poly -> Poly -> Poly
pnmul ps qs = remove_0s (map coeff idxs)
  where
      p = length ps
      q = length qs
      idxs = [0..p + q - 2]
      coeff k = sum [(ps !! i) * (qs !! (k - i)) | i <- [0..p - 1], let j = k - i, j >= 0, j < q]


pnsub :: Poly -> Poly -> Poly
pnsub ps qs = pnadd ps (pnmul [-1] qs)

pndiv :: Poly -> Poly -> (Poly, Poly)
pndiv _ [] = error "division by zero polynomial"
pndiv ps qs = helper ps [] 0
  where
    p = (length ps)
    q = (length qs)
    q0 = head qs
    helper current res i = if less current qs then (res, current) else
      helper (pnsub current (pnmul ([(head current) / q0] ++ (replicate (p - q - i) 0)) qs)) (res ++ [(head current) / q0]) (i + 1)

pndiff :: Poly -> Poly
pndiff [_] = []
pndiff ps = remove_0s (helper ps (length ps))
  where
    helper :: Poly -> Int -> Poly
    helper [] _ = []
    helper [_] _ = []
    helper (x:xs) n = (x * fromIntegral n):(helper xs (n - 1))

pnintg :: Poly -> Poly
pnintg ps = remove_0s (helper ps (length ps))
  where
    helper :: Poly -> Int -> Poly
    helper [] _ = [0]
    helper (x:xs) n = (x / fromIntegral n):(helper xs (n - 1))
