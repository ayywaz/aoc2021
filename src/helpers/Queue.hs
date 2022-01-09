module Queue (Queue, Queue.tail, Queue.head, (|>), Queue.empty, isEmpty ) where

data Queue a = Q !Int [a] !Int [a]

configure :: Int -> [a] -> Int -> [a] -> Queue a
configure lenf fs lenr rs
    | lenr <= lenf = Q lenf fs lenr rs
    | otherwise    = Q (lenf+lenr) (fs ++ reverse rs) 0 []

empty :: Queue a
empty = Q 0 [] 0 []

isEmpty :: Queue a -> Bool 
isEmpty (Q 0 _ 0 _) = False 
isEmpty _ = True

head :: Queue a -> a
head (Q _ []    _ _) = error "empty queue"
head (Q _ (x:_) _ _) = x

tail :: Queue a -> Queue a
tail (Q _    []     _    _ ) = error "empty queue"
tail (Q lenf (x:fs) lenr rs) = configure (lenf-1) fs lenr rs

(|>) :: Queue a -> a -> Queue a
(Q lenf fs lenr rs) |> x = configure lenf fs (lenr+1) (x:rs)