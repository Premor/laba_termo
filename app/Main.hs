module Main where
import qualified Data.Map.Lazy as Map
import Control.Parallel.Strategies
import Data.Csv as Csv
import qualified Data.ByteString.Char8 as C

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
	putStrLn "Кол-во итераций (по времени)"
	time <- getLine

	let _tx0 = read tx0 :: Float
	let	_tt0 = read tt0 :: Float
	let	_ttl = read ttl :: Float
	let	_l = read l :: Float
	let	_qwe = read qwe :: Int
	let _time = read time :: Int
	let h = _l / fromIntegral _qwe	
	let	start = replicate _qwe _tx0
	let result = make_shit _time start [_tt0,_ttl,h,_tx0]
	writeFile "./result.csv" $ to_csv "" result
	


to_csv = foldr outer
	where
	outer x acc 
		| acc == "" = inner x ++ acc
		| otherwise = inner x ++ "\n" ++ acc
		where
		inner x = foldr fun "" x
			where
			fun a acc2 
				| acc2 == "" = show a ++ acc2
				| otherwise = show a ++ ";" ++ acc2

pretty_print arg = mapM_ print arg

eval_p::[Float]->[Float]->[Float]
eval_p starts args = Map.elems $ (Map.mapWithKey tf starts_new `using` parTraversable rdeepseq) 
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
	|otherwise =  next_step:( make_shit (count - 1) next_step args)
		where
			[tt0,ttl,h,tx0] = args
			next_step = eval_p  starts args
			