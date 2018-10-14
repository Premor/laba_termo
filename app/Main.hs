module Main where
import qualified Data.Map.Lazy as Map

main :: IO ()
main = do
	putStrLn "Длина стержня"
	l <- getLine
	--putStrLn "Температура начальная"
	--tx0 <- getLine
	putStrLn "Температура нагретой стороны"
	tt0 <- getLine
	putStrLn "Температура среды"
	ttl <- getLine
	putStrLn "Кол-во разбиений"
	qwe <- getLine
	let test k a = a+k 
	print $ Map.mapWithKey test $ Map.fromList [(0,1),(1,1),(2,2)]
	let _tx0 = 100-- read tx0 :: Float
	let	_tt0 = read tt0 :: Float
	let	_ttl = read ttl :: Float
	let	_l = read l :: Float
	let	_qwe = read qwe :: Int
	let h = _l / fromIntegral (_qwe - 1)	
	let	start = replicate _qwe _tx0
	-- let make_shit 0 starts = eval_p
	--     make_shit count starts = eval_p : make_shit (count - 1) eval_p
	-- 	where
	-- 	eval_p = myMap tf $ replicate (length starts) 0
	-- 		where
	-- 		tf e c
	-- 			| c == 0 = _tt0
	-- 			| c == length starts = _ttl
	-- 			| otherwise = ((starts !! (c-1)) - 2*(starts !! c) + (starts !! (c+1))) / h^2
	print start
	let result = make_shit 50 start [_tt0,_ttl,h,_tx0]
	
	pretty_print result
	putStrLn "132"


myMap :: (a->Int->a)->[a]->Int->[a]
myMap fun [x] count = [fun x count]
myMap fun (x:xs) count = fun x count:myMap fun xs (count + 1) 	


pretty_print arg = mapM_ print arg

eval_p::[Float]->[Float]->[Float]
eval_p starts args = Map.elems $ Map.mapWithKey tf starts_new
	where
		starts_new = Map.fromList $ zip [0..] starts
		[tt0,ttl,h,_] = args
		tf k e
			| k == 0 = tt0
			| k == (length starts) - 1  = ttl
			| otherwise = e + ((starts_new Map.! (k-1)) - 2*(starts_new Map.! k) + (starts_new Map.! (k+1))) / h^2
						
eval_f len tx0 = replicate len tx0

make_shit:: Int->[Float]->[Float]->[[Float]]
make_shit count starts args
	|count == 0 = [eval_p  starts args]
	|otherwise = (eval_p  starts args):( make_shit (count - 1) (eval_p  starts args) args)
		where
			[tt0,ttl,h,tx0] = args
			