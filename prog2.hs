import Control.Applicative
import Control.Monad

data Frac = Cons Double | Frac (Frac) (Frac) deriving (Show, Eq)


data Result a = Exc String | Ret a deriving (Show)

instance Functor Result where
	fmap f (Ret a) = Ret (f a)
	fmap _ (Exc s) = Exc s

instance Applicative Result where
	pure x = Ret x
	(<*>) (Ret f) (Ret x) = Ret (f x)
	(<*>) _ (Exc s) = Exc s
	(<*>) (Exc s) _ = Exc s

instance Monad Result where
	return x = Ret x
	(Ret x) >>= f = f x
	(Exc s) >>= _ = Exc s

eval :: Frac -> Double
eval (Cons x) = x
eval (Frac f1 f2) = (eval f1) / (eval f2)

--eval1 :: Frac -> Result a
eval1 (Cons x) = Ret x
eval1 (Frac f1 f2) = let m = eval1 f1
                         n = eval1 f2
                    in (case m of Exc s -> Exc s
                                  Ret m' -> case n of Exc s' -> Exc s'
                                                      Ret n' -> if n' == 0 then Exc "Division by Zero" else Ret (m'/n'))

eval2 (Cons x) = Ret x
eval2 (Frac f1 f2) = 
	(eval2 f1) >>= (\x ->
	(eval2 f2) >>= (\y ->
	if y == 0 then Exc "Division by zero" else Ret (x/y)))

-- works okay
f = Frac (Frac (Cons 1) (Cons 2)) (Cons 2)
f' = eval f

f1 = Frac (Frac (Cons 1) (Cons 2)) (Cons 0)
f1' = eval2 f1