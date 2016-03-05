import Control.Applicative
import Control.Monad

main = do
	getLine >>= (\x -> putStrLn $ x++"!")

main' = do
	line <- getLine
	putStrLn $ reverse line

seqA :: (Applicative f) => [f a] => f [a]
seqA l = foldr (liftA2 (:)) (pure []) l

data Pole = Pole (Int, Int) deriving (Show)

--instance Show Pole where
--    show (Pole (a,b)) = "("++show a++", "++show b++")"


landL :: Int -> Pole -> Maybe Pole
landL n (Pole (a,b)) = if (abs $ a+n - b) < 4 then Just $ Pole (a+n, b) else Nothing

landR :: Int -> Pole -> Maybe Pole
landR n (Pole (a,b)) = if (abs $ b+n - a) < 4 then Just $ Pole (a, b+n) else
    Nothing

p1 = landL 5 $ Pole (0, 0)
p2 = landL 2 $ Pole (0, 0)
--p3 = p2 >>= (\x -> x)