import Control.Parallel

main = print $ parallel_map (+1) [1..10000]  

mymap :: (a -> b) -> [a] -> [b]
mymap f [] = []
mymap f (x:xs) = f x : mymap f xs

parallel_map :: (a -> b) -> [a] -> [b]
parallel_map f [] = []
--parallel_map f (x:xs) = --par nf f x : nf
                          --  where nf = parallel_map f xs

                    --    par nf1 (pseq nf2 (nf1:nf2))
                    --        where   nf1 = f x
                    --                nf2 = parallel_map f xs
                                    

-- WE WANT TO (IN PARALLEL)
--  CALL map f xs

--  COMPUTE f x


parallel_map f (x:xs) = par (force recursive_call) (function_call : recursive_call)
                            where function_call = f x  
                                  recursive_call = parallel_map f xs


-- THEN APPEND THE RESULS

force :: [a] -> ()
force (x:xs) = x `pseq` force xs
force _ = ()