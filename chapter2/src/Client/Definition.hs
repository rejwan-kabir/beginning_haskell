{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE ParallelListComp  #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TransformListComp #-}
{-# LANGUAGE ViewPatterns      #-}

module Client.Definition where

import           Data.Char
import           Data.Foldable
import           Data.List
import qualified Data.Map      as M
import qualified Data.Set      as S
import           GHC.Exts

data Gender
  = Male
  | Female
  | Unknown
  deriving (Show, Eq, Ord)

data Person =
  Person String
         String
         Gender
  deriving (Show, Eq, Ord)

data Client
  = GovOrg String
  | Company String
            Integer
            Person
            String
  | Individual Person
               Bool
  deriving (Show, Eq, Ord)

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

data PersonR = PersonR
  { firstName :: String
  , lastName  :: String
  } deriving (Show)

data ClientR
  = GovOrgR { clientRName :: String }
  | CompanyR { clientRName :: String
             , companyId   :: Integer
             , person      :: PersonR
             , duty        :: String }
  | IndividualR { person :: PersonR }
  deriving (Show)

greet :: ClientR -> String
greet IndividualR {person = PersonR {firstName = fn}} = "Hi, " ++ fn
greet CompanyR {clientRName = c}                      = "Hello, " ++ c
greet GovOrgR {clientRName = c}                       = "Welcome, " ++ c

greet' :: ClientR -> String -- use NamedFieldPuns
greet' IndividualR {person = PersonR {firstName}} = "Hi, " ++ firstName
greet' CompanyR {clientRName}                     = "Hello, " ++ clientRName
greet' GovOrgR {clientRName}                      = "Welcome, " ++ clientRName

greet'' :: ClientR -> String -- use RecordWildCards
greet'' IndividualR {person = PersonR {..}} = "Hi, " ++ firstName
greet'' CompanyR {..}                       = "Hello, " ++ clientRName
greet'' GovOrgR {..}                        = "Welcome, " ++ clientRName

nameInCapitals :: PersonR -> PersonR -- caseClass.copy in scala
nameInCapitals p@(PersonR {firstName = initial:rest}) =
  let newName = (toUpper initial) : rest
  in p {firstName = newName}
nameInCapitals p@(PersonR {firstName = ""}) = p

sayHello :: [String] -> [String]
sayHello =
  map
    (\name ->
       case name of
         "shuvo" -> "Hi shuvo"
         _       -> "Fuck off")

sayHello' :: [String] -> [String] -- use LambdaCase
sayHello' =
  map
    (\case
       "shuvo" -> "Hi shuvo"
       _ -> "Fuck off")

uncurry f (x, y) = f x y

curry f x y = f (x, y)

data InfNumber a
  = NegativeInf
  | Number a
  | PositiveInf
  deriving (Show)

infMax :: (Ord a) => InfNumber a -> InfNumber a -> InfNumber a
infMax x NegativeInf         = x
infMax NegativeInf x         = x
infMax _ PositiveInf         = PositiveInf
infMax PositiveInf _         = PositiveInf
infMax (Number x) (Number y) = Number (max x y)

elem' :: (Eq a) => a -> [a] -> Bool
x `elem'` xs =
  case find (== x) xs of
    Just _  -> True
    Nothing -> False

transformAfterComprehension = [x * y | x <- [1 .. 4], y <- [5 .. 9], then reverse] -- needs TransformListComp

transformUsingAfterComprehension -- the [1,1,1] == 1, the [1,1,2] throws exception
 = [(the p, m) | x <- [-1, 1, -2], y <- [1, 2, 3], let m = x * y, let p = m > 0, then group by p using groupWith]

parallelListComp = [x * y | x <- [1, 2, 3] | y <- [1, 2, 3]] -- returns [1,4,9]

minSort :: (Ord a) => [a] -> [a]
minSort =
  unfoldr
    (\case
       [] -> Nothing
       xs -> Just (m, delete m xs)
         where m = minimum xs)

foldr2 :: (Maybe (a, b) -> b) -> [a] -> b
foldr2 f []     = f Nothing
foldr2 f (x:xs) = f $ Just (x, foldr2 f xs)

mapAsFold2 :: (a -> b) -> [a] -> [b]
mapAsFold2 f =
  foldr2
    (\case
       Nothing -> []
       Just (x, xs) -> f x : xs)

data ClientKind
  = GovOrgKind
  | CompanyKind
  | IndividualKind
  deriving (Eq, Ord)

classifyClients1 :: [Client] -> M.Map ClientKind (S.Set Client)
classifyClients1 = foldl f M.empty
  where
    g :: Client -> ClientKind -> M.Map ClientKind (S.Set Client) -> M.Map ClientKind (S.Set Client)
    g client kind acc =
      case M.lookup kind acc of
        Nothing  -> M.insert kind S.empty acc
        Just set -> M.insert kind (S.insert client set) acc
    f :: M.Map ClientKind (S.Set Client) -> Client -> M.Map ClientKind (S.Set Client)
    f acc client =
      case client of
        GovOrg {}     -> g client GovOrgKind acc
        Company {}    -> g client CompanyKind acc
        Individual {} -> g client IndividualKind acc

-- classifyClients2 takes more time, but less space
classifyClients2 :: [Client] -> M.Map ClientKind (S.Set Client)
classifyClients2 clientList = M.map S.fromList $ foldl f M.empty clientList
  where
    g :: Client -> ClientKind -> M.Map ClientKind [Client] -> M.Map ClientKind [Client]
    g client kind acc =
      case M.lookup kind acc of
        Nothing   -> M.insert kind [] acc
        Just list -> M.insert kind (client : list) acc
    f :: M.Map ClientKind [Client] -> Client -> M.Map ClientKind [Client]
    f acc client =
      case client of
        GovOrg {}     -> g client GovOrgKind acc
        Company {}    -> g client CompanyKind acc
        Individual {} -> g client IndividualKind acc
