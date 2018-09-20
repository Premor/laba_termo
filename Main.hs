module Main where

main :: IO ()
main = do
	putStrLn "Длина стержня"
	l <- getLine
	putStrLn "Температура начальная"
	tx0 <- getLine
	putStrLn "Температура нагретой стороны"
	tt0 <- getLine
	putStrLn "Температура среды"
	ttl <- getLine
	putStrLn "Кол-во разбиений"
	qwe <- getLine
	
	let _tx0 = read tx0 :: Float
	let	_tt0 = read tt0 :: Float
	let	_ttl = read ttl :: Float
	let	_l = read l :: Float
	let	_qwe = read qwe :: Int
	let h = _l / _qwe
	let	start = replicate _qwe _tx0
	let result = make_shit 10 _qwe _tx0 _ttl start

	let make_shit count start = eval_p : make_shit (count - 1) eval_p
		where
		eval_p = myMap tf $ replicate (length start) 0.0 
			where
			tf e c
				| c == 0 = _tt0
				| c == length start = _ttl
				| otherwise = ((start !! (c-1)) - 2*(start !! c) + start !! (c+1)) / h^2

	
	print start



myMap :: (a->Int->a)->[a]->Int->[a]
myMap fun (x:xs) count = fun x count:myMap fun xs (count + 1) 	
