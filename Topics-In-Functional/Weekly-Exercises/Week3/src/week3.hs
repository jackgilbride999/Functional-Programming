import Control.Monad

main = putStr "week3"

data List a = Nil | Cons a (List a)

instance Functor (List) where
     --fmap = liftM
     -- fmap :: (a -> b) -> f a -> f b
     fmap f Nil = Nil
     fmap f (Cons x xs) = Cons (f x) (fmap f xs)


instance Applicative (List) where
     -- pure :: a -> f a
     pure x = Cons x Nil

     -- (<*>) :: List (a -> b) -> List a -> List b
     _ <*> Nil = Nil
     Nil <*> _ = Nil
     Cons f fs <*> Cons x xs = Cons (f x) (fs <*> xs)

instance Monad (List) where
     -- (>>=) :: List a -> (a -> List b) -> List b
      Nil >>= f = Nil
      (Cons x xs) >>= f = f x
      