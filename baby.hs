
doubleMe x = x + x
doubleUs x y = x * 2 + y * 2
boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

sumProd :: [[Integer]] -> Integer
sumProd [[]] = 0
sumProd l1 = sum [product [ x | x <- l2] | l2 <- l1]

sumProd2 :: [[Integer]] -> Integer
sumProd2 l1 = sum (map product (filter (/=[]) l1))

sumProd3 :: [[Integer]] -> Integer
sumProd3 l1 = sum $ map  product $ filter (/=[])  l1

sumProd4 :: [[Integer]] -> Integer
sumProd4 l1 = (sum . (map product).(filter (/=[]))) l1

sumProd5 :: [[Integer]] -> Integer
sumProd5 l1 = sum . map product $  filter (/=[]) l1

type Trip = (String, Integer, Float)
type Tour = [Trip]

discount :: Tour -> Integer -> Tour
discount tour len = (map (\ (a, b, x) -> (a, b, (x - x/10))) (filter (\(a,b,x) -> b > len) tour)) ++ (filter (\(a,b,x) -> b <= len) tour)

discount2 :: Tour -> Integer -> Tour
discount2 tour len = [(a,b, 0.9 * x) | (a,b,x) <- tour, b > len ] ++ [(a,b,c) | (a,b,c) <- tour, b <= len] 


funckyNumbers :: Integer -> String
funckyNumbers a = if (b /= []) then "YES" else "NO"
					where b = [x + y | x <-numbers , y <- numbers, x + y == a]
						where numbers = [ x*(x + 1) `div` 2 | x <- [1..a], x*(x + 1) `div` 2 <= a]