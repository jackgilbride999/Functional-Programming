
main = putStr "Week 3. Call any function here."

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
          = join (fmap f (Cons x Nil))
          = join (Cons (f x) (fmap f Nil))
          = join (Cons (f x) (Nil))
          = cat (f x) (join Nil)
          = cat (f x) (Nil)
          Case 1: f x = Nil
          = cat (Nil) Nil
          = Nil
          Case 2: f x = Cons y ys
          = cat (Cons y ys) (Nil)
          = Cons y (cat ys Nil)
          (The above patten will continue until the list ys is fully expanded.
           Every list ends with Nil by definition, so eventually each y will be
           brought outside the cat into the Cons notation, and the cat function 
           will contain the Nil from the end of ys and the other Nil, which will
           evaluate to Nil. So the entire thing will evaluate to Cons y ys, which
           is equal to f x as required)

     Law 2: Right Identity
     m >>= return behaves the same as m
          m >== return
          = join (fmap return m)
          Case 1: m = Nil
          = join (fmap return Nil)
          = join (Nil)
          = Nil
          Case 2: m = Cons x xs
          = join (fmap return (Cons x xs))
          = join (Cons (return x) fmap (return xs))
          (fmap will map return to each x in the list. Each return x will evaluate to pure x, 
          which will evaluate to Cons x Nil. join will iterate through these sublists and join 
          them into one single list containing all elements from the sublists. i.e., it will take
          all the xs and return them in the list monad, which is exactly Cons x xs, which is 
          exactly m.)


     Law 3: Associativity 
     (m >>= f) >>= g behaves the same as
     m >>= (\x -> f x >>= g)     
          
          (m >>= f) >>= g
          = (join (fmap f m)) >>= g
          = (join (fmap g (join (fmap f m))))

          m >>= (\x -> f x >>= g)
          = join (fmap (\x -> f x >>= g) m)
          = join (fmap (join (fmap g (\x -> f x))) m)
          
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

{-
instance Applicative (Pair a) where

     -- pure :: b -> f b
     pure y = P y y

     -- (<*>) :: List (a -> b) -> List a -> List b
     P x1 f <*> P x2 y = fmap f (P x2 y)
-}
{-
     The attempt at giving an instance of Applicative for Pair is above, commented out to avoid a compile time error.

     We can create an instance of Functor for Pair because we can create an instance of it, and an implementation for fmap, 
     which are of the correct type. For a (Pair a b), we can say that b is the value wrapped in the Functor. Therefore (Pair a) 
     is the instance of the Functor. fmap then, applies the function f to the wrapped value b, inside the Functor.

     We cannot create an instance of Applicative for Pair because we cannot define pure. pure takes a value and returns it wrapped 
     in the Applicative. So it needs to take (b), and return (P a b). So pure must return a Functor containing a value of type (a), 
     but it has no indication of what type (a) is from the input. If it returned (P b b), this would confine the types in the pair, 
     which is not how the pair was defined. We cannot encode a pure function, so we cannot create an instance of an applicative.

     We could also try to encode <*>. This is possible where the first Applicative holds a function in its value (b), and the second 
     holds the value to apply it to. However, both these applicatives may have different (a) values. The function would have to arbitrarily
     choose whether to return a Pair with the (a) value from the first applicative or the second.

     Even if we could encode an Applicative in Haskell without defining the pure function, it needs to be applied to prove the Applicative Functor 
     Laws, as it is used in all of them. So without pure, there is no Applicative.
-}