{-# LANGUAGE ViewPatterns #-}

module Client.Definition where

data Gender
  = Male
  | Female
  | Unknown
  deriving (Show)

data Person =
  Person String
         String
         Gender
  deriving (Show)

data Client
  = GovOrg String
  | Company String
            Integer
            Person
            String
  | Individual Person
               Bool
  deriving (Show)

clientName :: Client -> String
clientName client =
  case client of
    GovOrg name -> name
    Company name id person resp -> name
    Individual person ads ->
      case person of
        Person fname lname gender -> fname ++ " " ++ lname

companyName :: Client -> Maybe String
companyName client =
  case client of
    Company name _ _ _ -> Just name
    _                  -> Nothing

fibonacci :: Integer -> Integer
fibonacci n =
  case n of
    0 -> 0
    1 -> 1
    _ -> fibonacci (n - 1) + fibonacci (n - 2)

f :: Client -> String
f client =
  case client of
    Company _ _ (Person name _ _) "Boss" -> name ++ " is the boss"
    _                                    -> "There is no boss"

g :: Client -> String
g client =
  case client of
    Company _ _ (Person name _ _) pos ->
      case pos of
        "Boss" -> name ++ " is the boss"
    _ -> "There is no boss"

clientName1 :: Client -> String
clientName1 (GovOrg name)                         = name
clientName1 (Company name _ _ _)                  = name
clientName1 (Individual (Person fname lname _) _) = fname ++ lname

fibonacci1 :: Integer -> Integer
fibonacci1 0 = 0
fibonacci1 1 = 1
fibonacci1 n = fibonacci1 (n - 1) + fibonacci1 (n - 2)

(+++) :: [a] -> [a] -> [a]
[] +++ ls2 = ls2
(l:ls1) +++ ls2 = l : (ls1 +++ ls2)

sorted :: (Ord a) => [a] -> Bool
sorted []       = True
sorted [_]      = True
sorted (x:y:zs) = x < y && sorted (y : zs)

maxmin :: (Ord a) => [a] -> (a, a)
maxmin [x] = (x, x)
maxmin (x:xs) =
  let (xs_max, xs_min) = maxmin xs
  in ( if x > xs_max
         then x
         else xs_max
     , if x < xs_min
         then x
         else xs_min)

fibonacci2 :: Integer -> Maybe Integer
fibonacci2 n
  | n < 0 = Nothing
fibonacci2 0 = Just 0
fibonacci2 1 = Just 1
fibonacci2 n =
  let (Just f1, Just f2) = (fibonacci2 (n - 1), fibonacci2 (n - 2))
  in Just (f1 + f2)

binom :: Integer -> Integer -> Integer
binom _ 0 = 1
binom x y
  | x == y = 1
binom n k = binom (n - 1) (k - 1) + binom (n - 1) k

specialClient :: Client -> Bool
specialClient (clientName -> "Rejwan Shuvo") = True -- set ViewPatterns
specialClient _                              = False
