module Domain where

import ClassyPrelude
import Text.Regex.PCRE.Heavy

data Auth = Auth {
      authEmail :: Email
    , authPassword :: Password
} deriving (Show, Eq)

data RegistrationError
  = RegistrationErrorEmailTaken
  deriving (Show, Eq)

newtype Email = Email {emailRaw :: Text} deriving (Show, Eq)

rawEmail :: Email -> Text
rawEmail = emailRaw

mkEmail :: Text -> Either [Text] Email
mkEmail = validate Email [regexMatches
  [re|^[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,64}$|]
  "Not a vaild email"
  ]

newtype Password = Password {passwordRaw :: Text}  deriving (Show, Eq)

rawPassword :: Password -> Text
rawPassword = passwordRaw

mkPassword :: Text -> Either [Text] Password
mkPassword = validate Password
  [ lengthBetween 5 50 "Should between 5 and 50"
  , regexMatches [re|\d|] "Should contain number"
  , regexMatches [re|[A-Z]|] "Should contain uppercase letter"
  , regexMatches [re|[a-z]|] "Should contain lowercase letter"
  ]


mkAuth :: Text -> Text -> Either [Text] Auth
mkAuth email password = liftA2 Auth (mkEmail email) (mkPassword password)

data EmailValidationErr = EmailValidationErrInvaildEmail

data PasswordValidationErr = PasswordValidationErrLength Int
    | PasswordValidationErrMustContainUpperCase
    | PasswordValidationErrMustContainLowerCase
    | PasswordValidationErrMustContainNumber


type Validation e a = a -> Maybe e

validate :: (a -> b) -> [Validation e a] -> a -> Either [e] b
validate contructor validators input =
  case concatMap (\ f -> maybeToList $ f input) validators of
    [] -> Right $ contructor input
    errs -> Left errs

rangeBetween :: (Ord a) => a -> a -> e -> Validation e a
rangeBetween mixRange maxRange msg a = if a < mixRange || a > maxRange then Just msg else Nothing

lengthBetween :: (MonoFoldable a) => Int -> Int -> e -> Validation e a
lengthBetween minLen maxLen msg val = rangeBetween minLen maxLen msg (length val)

regexMatches :: Regex -> e -> Validation e Text
regexMatches regex msg val =
  if val =~ regex then Nothing else Just msg
