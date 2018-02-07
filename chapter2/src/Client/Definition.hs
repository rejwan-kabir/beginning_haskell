module Client.Definition where

data Gender = Male | Female | Unknown deriving (Show)
data Person = Person String String Gender deriving (Show)
data Client = GovOrg String
            | Company String Integer Person String
            | Individual Person Bool
            deriving (Show)

clientName :: Client -> String
clientName client =
  case client of
    GovOrg name -> name
    Company name id person resp -> name
    Individual person ads ->
      case person of Person fname lname gender -> fname ++ " " ++ lname

companyName :: Client -> Maybe String
companyName client =
  case client of
    Company name _ _ _ -> Just name
    _ -> Nothing