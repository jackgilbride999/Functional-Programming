
main = putStr "week3"

data List a = Nil | Cons a (List a)

-- Helper functions to help the monadic operations.
-- Use join and cat to transform lists of lists into one single list.
-- Recursively reach into all elements with join and pull them out to the
-- same level. Use cat to append them all to the one list.
join :: List (List a) -> List a
join Nil = Nil
join (Cons xs xss) = cat xs (join xss)

cat :: List a -> List a -> List a
cat Nil ys = ys
cat (Cons x xs) ys = Cons x (cat xs ys)

instance Functor (List) where
     -- fmap :: (a -> b) -> f a -> f b
     -- Apply the function f across the entire list.
     -- If the list is empty, return the empty list.
     -- If the list contains an item, call f on the item, wrap it in the list and recursively call for the rest of the input list.
     fmap f Nil = Nil
     fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative (List) where
     -- pure :: a -> f a
     -- Take an argument and wrap it in the list.
     -- There are no other elements given for the rest of the list, so the tail of the list is empty.
     pure x = Cons x Nil

     -- (<*>) :: List (a -> b) -> List a -> List b
     -- Sequential application function. 
     -- Takes both a function and a value, both wrapped in the applicative, and returns the function applied
     -- to the value, wrapped in the applicative. The infix notation allows a series of sequential operations
     -- to be nicely applied with this.
     -- In this case the applicative is a list, so there may be multiple functions and multiple values. Apply
     -- all functions to all values and concatenate them into a single list.
     _ <*> Nil = Nil
     Nil <*> _ = Nil
     (Cons f fs) <*> Cons x xs = cat (fmap f (Cons x xs)) (fs <*> (Cons x xs))

instance Monad (List) where
     -- (>>=) :: List a -> (a -> List b) -> List b
      list >>= f = join (fmap f list)

{-
     Three Monad Laws:

     Law 1: Left Identity
     return x >>= f behaves the same as f x
          return x >>= f
          = pure x >>= f           -- as return = pure by default
          = Cons x Nil >>= f       -- by the above definition for pure
          

     Law 2: Right Identity
     m >>= return behaves the same as m
          m >== return
          


     Law 3: Associativity 
     (m >>= f) >>= g behaves the same as
     m >>= (fun x -> f x >>= g)     
     	(m >>= f) >>= g
          = (f m) >>= g            -- Law 1
          = g (f m)                -- Law 1

          m >>= ((\x -> f x) >>= g)
          = m >>= (g (\x -> f x))  -- Law 1
          = (g (\x -> f x)) m      -- Law 1
          
-}

data Pair a b = P a b

instance Functor (Pair a) where
     -- fmap :: (b -> c) -> f b -> f c
     fmap f (P x y) = P x (f y)
{-
     Law 1: Identity
     fmap id behaves the same as id

     fmap id (P a b)
     = P a (id b)        -- by definition of fmap above
     = P a (b)           -- by definition of id
     = P a b             -- removing unneeded bracket, no change to value
     = (P a b)           -- wrapping in brackets, no change to value
     = id (P a b)        -- by definition  of id
     Q.E.D.

     Law 2: Composition
     fmap (f . g) behaves the same as fmap f . fmap g
     
     LHS:
     fmap (f . g) (P a b)
     = fmap (\x -> f (g x)) (P a b)     -- Definition for (.)
     = P a ((\x -> f g x) b)            -- Definition for fmap above
     = P a (f g b)                      -- Application of lambda function

     RHS
     (fmap f . fmap g) (P a b)
     = (\x -> fmap f (fmap g)) (P a b)  -- Definition of (.)
     = fmap f (fmap g (P a b))          -- Application of lambda function
     = fmap f (P a (g b))               -- Definition for fmap above
     = P a (f (g b))                    -- Definition for fmap above
     = P a (f g b)                      -- Removing unneeded bracket, no change to value

     LHS = RHS, so equation holds.
     Q.E.D.
-}

