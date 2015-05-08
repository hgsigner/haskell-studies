lucky :: (Integral a) => a -> String
lucky 7 = "Heyy Seven!"
lucky x = "Ops. You are not seven."

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial(n - 1)

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

addVectors :: (Num a) => (a,a) -> (a,a) -> (a,a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first :: (a,b,c) -> a
first (x,_,_) = x

capital :: String -> String
capital "" = "Oops. No string passed"
capital all@(x:xs) = "The first letter for " ++ all ++ "is the letter: " ++ [x]

guardTest :: (Integral a) => a -> String
guardTest gt
	      | gt <= 1   = "Guard number 1."
	      | gt <= 2   = "Guard number 2."
	      | otherwise = "Unkown guard."

whereTest :: (Integral a) => a -> a -> String
whereTest a b
        | final <= 3 = "LEss"
        | final <= 6 = "Six"
        where final = a + b

calcBmi :: (RealFloat a) => [(a, a)] -> [a]
calcBmi xs = [ bmi | (w, h) <- xs, let bmi = w / h ^ 2 ]