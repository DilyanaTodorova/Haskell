	
funckyNumbers :: Integer -> String
funckyNumbers a = if (b /= []) then "YES" else "NO"
					where b = [x + y | x <-numbers , y <- numbers, x + y == a]
						where numbers = [ x*(x + 1) `div` 2 | x <- [1..a], x*(x + 1) `div` 2 <= a]
							