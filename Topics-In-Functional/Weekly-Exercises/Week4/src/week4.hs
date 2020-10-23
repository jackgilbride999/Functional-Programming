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