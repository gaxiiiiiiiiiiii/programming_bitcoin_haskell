{-# LANGUAGE BangPatterns #-}

module Utils where


exgcd :: Integral c => c -> c -> (c, c, c)
exgcd f1 f2 = f $ go f1 f2 1 0 0 1
  where
    go !r0 !r1 !s0 !s1 !t0 !t1
      | r1 == 0   = (r0, s0, t0)
      | otherwise = go r1 r2 s1 s2 t1 t2
      where
        (q, r2) = r0 `divMod` r1
        s2 = s0 - q*s1
        t2 = t0 - q*t1
    f (g,u,v)
      | g < 0 = (-g, -u, -v)
      | otherwise = (g,u,v)