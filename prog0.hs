import Control.Applicative

half x = if even x
			then Just (x `div` 2)
			else Nothing


main = do
	let f = (fmap (+) [1..3]) <*> [1..5]

	print f