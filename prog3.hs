import Control.Applicative
import Control.Monad
import Data.List

solveRPN :: String -> Float
solveRPN exp = head $ foldl foo [] $ words exp
    where   foo (x:y:ys) "*" = (x*y):ys
            foo (x:y:ys) "+" = (x+y):ys
            foo (x:y:ys) "-" = (x-y):ys
            foo (x:y:ys) "^" = (y**x):ys
            foo (x:xs) "ln" = (log x):xs
            foo xs "sum" = [sum xs]
            foo s n = read n:s

read' :: (Read a, Eq a) => String -> Maybe a
read' s = case reads s of [(x,"")] -> Just x; _ -> Nothing

foo :: [Float] -> String -> Maybe [Float]
foo (x:y:ys) "*" = return ((x*y):ys)
foo s n = liftM (:s) (read' n) 

solveRPN1 :: String -> Maybe Float
solveRPN1 exp = (foldM foo [] $ words exp) >>= (\x -> return $ head x)
--solveRPN1 exp = do 
--    res <- (foldM foo [] $ words exp)
--    return $ head res