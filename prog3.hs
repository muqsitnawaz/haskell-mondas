import Control.Applicative
import Control.Monad

solveRPN :: String -> Float
solveRPN exp = head $ foldl foo [] $ words exp
    where   foo (x:y:ys) "*" = (x*y):ys
            foo (x:y:ys) "+" = (x+y):ys
            foo (x:y:ys) "-" = (x-y):ys
            foo (x:y:ys) "^" = (y**x):ys
            foo (x:xs) "ln" = (log x):xs
            foo xs "sum" = [sum xs]
            foo s n = read n:s

--read' :: (Read a) => String -> Maybe a
--read' s = 

readMaybe :: (Read a) => String -> Maybe a  
readMaybe st = 
    case reads st of [(x,"")] -> Just x  
                            _ -> Nothing

--solveRPN1 :: String -> Maybe Float
--solveRPN1 exp = head $ foldl (\x -> x) [] $ words exp
--    where   foo (x:y:ys) "*" = do x >>= (\x' -> (y >>= (\y' -> return (x'*y'))))):ys
--            foo s n = read n:s