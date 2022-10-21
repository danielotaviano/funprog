module LC where


(+++) :: [a] -> [a] -> [a]
(+++) []       xs = xs
(+++) (a : as) xs = a : (as +++ xs)

