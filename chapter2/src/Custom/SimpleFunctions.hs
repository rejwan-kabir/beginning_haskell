module Custom.SimpleFunctions where

firstOrEmpty :: [String] -> String
firstOrEmpty ls = if not (null ls) then head ls else "empty"

(+++) :: [a] -> [a] -> [a]
ls1 +++ ls2 = if null ls1
                then ls2
                else (head ls1) : (tail ls1 +++ ls2)

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []