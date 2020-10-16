main = putStr "week3"

data List a = Nil | Cons a (List a)

instance Monad List where
      (>>=) :: List a -> (a -> List b) -> List b
      Nil >>= k = Nil
      (Cons a as) >>= k = Cons a (k as)

      return :: a -> List a
      return a = Cons a