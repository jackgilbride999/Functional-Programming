main = putStr "week4"

{-
    PART 1:
    A common pattern in programming is to need some form of logging or auditing for a process. 
    In Haskell this is sometimes captured by the writer monad. Writer can be thought of as a variant 
    of the State monad -- it maintains a state, which represents a kind of journal that the monadic 
    computation can modify, but instead of "get" and "put" operations which allow the computation to 
    inspect and arbitrarily update the state there is only one operation, tell, which appends a new 
    entry to the journal. At the end of the computation there is a final value as usual and also the 
    accumulated journal of log entries. 

    In this exercise you will design and implement a version of the writer monad. While the Writer 
    can be parameterised so that the journal it's keeping is of arbitrary type you can just assume 
    that it's a list of strings for this exercise.

    For example, running this 

    example :: Writer Int
    example = do
        tell "entry 1"
        tell "entry 2"
    return (1 + 1)

    should deliver (["entry 1", "entry 2"], 2)
-}

-- Running this delivers (["entry 1", "entry 2"], 2) as expected
example = do
    tell "entry 1"
    tell "entry 2"
    return (1 + 1)

-- The wrapper Writer holds a value a and a log w
data Writer w a = Writer [String] a deriving Show

instance Functor (Writer w) where
    -- fmap :: (a -> b) -> Writer w a -> Writer w b
    -- Apply f to the wrapped value
    fmap f (Writer w x) = Writer w (f x)

instance Applicative (Writer w) where
    -- pure :: a -> f a
    -- Wrap a and return it. There is no sensible item to put in the log.
    pure x = Writer [] x

    -- (<*>) :: Writer (a -> b) -> Writer a -> Writer b
    -- <*> allows us to apply a wrapped function to a wrapped value without having to 
    -- reach into the Applicative each time.
    -- There is no reason to drop the log from either Writer, so concatenate them together 
    -- in the result.
    Writer w1 f <*> Writer w2 x = Writer (w1 ++ w2) (f x)

instance Monad (Writer w) where
    -- (>>=) :: Writer w a -> (a -> Writer w b) -> Writer w b
    -- Bind allows us to string function applications to monads together.
    -- To bind (Writer w x) with f, pass x to f. f takes a value and returns a Writer, so 
    -- f x is a Writer with a new log and new value.
    -- But we don't want to lose the old log, we want to concatenate it to our new log.
    -- To do this we can make use of the <*> function, where the value in the first writer is
    -- the identity function, so as not to change the value wrapped in f x.
    Writer w x >>= f = (Writer w id) <*> f x

-- Tell just appends a new entry to the journal. It returns no interesting value.
tell :: String -> Writer [String] ()
tell w = Writer [w] ()

{-
    PART 2:
    There are two ways you could generalize this monad:

    Have the log be a list of any type, which would allow the programmer to create a type for individual log entries
    Have the log be of any type at all, letting the programmer create a type for the log overall.
    If you want to take the second approach you will run into a problem with your declarations as they stand. You don't 
    have to solve the problem for this exercise, but try making the log type a parameter and determine what the problem 
    is (hint: it is related to the operations that the Writer must perform on the log itself). 

    For part 2, just write a short note on what the problem you run into is, and speculate on how to solve it.
-}