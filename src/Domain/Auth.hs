module Domain.Auth where

import Control.Monad.Except
import ClassyPrelude
import Text.Regex.PCRE.Heavy

import Domain.Validation

data Auth = Auth {
      authEmail :: Email
    , authPassword :: Password
} deriving (Show, Eq)

data RegistrationError
  = RegistrationErrorEmailTaken | PasswordValidationErr
  deriving (Show, Eq)

type VerificationCode = Text
type UserId = Int
type SessionId = Text

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

data EmailVerificationError = EmailVerificationErrorInvalidCode deriving (Show, Eq)

data LoginError = LoginErrorInvalidAuth | LoginErrorEmailNotVerified deriving (Show, Eq)

class Monad m => AuthRepo m where
    addAuth :: Auth -> m (Either RegistrationError VerificationCode)
    setEmailAsVerified :: VerificationCode -> m (Either EmailVerificationError ())
    findUserByAuth :: Auth -> m (Maybe (UserId, Bool))

class Monad m => EmailVerificationNotif m where
    notifyEmailVerfication :: Email -> VerificationCode -> m ()


class Monad m => SessionRepo m where
    newSession :: UserId -> m SessionId

register :: (AuthRepo m, EmailVerificationNotif m)
    => Auth -> m (Either RegistrationError ())
register auth = runExceptT $ do
    vCode <- ExceptT $ addAuth auth
    let email = authEmail auth
    lift $ notifyEmailVerfication email vCode

verifyEmail :: AuthRepo m => VerificationCode -> m (Either EmailVerificationError ())
verifyEmail = setEmailAsVerified

login :: (AuthRepo m, SessionRepo m) => Auth -> m (Either LoginError SessionId)
login auth = runExceptT $ do
    result <- lift $ findUserByAuth auth
    case result of
        Nothing -> throwError LoginErrorInvalidAuth
        Just (_, False) ->  throwError LoginErrorEmailNotVerified
        Just (userId, True) -> lift $ newSession userId


instance AuthRepo IO where
    addAuth (Auth email password) = do
        putStrLn $ "adding auth: " <> rawEmail email
        return $ Right "fake verification code"
    setEmailAsVerified = undefined
    findUserByAuth = undefined

instance EmailVerificationNotif IO where
    notifyEmailVerfication email vcode =
        putStrLn $ "Notify" <> rawEmail email <> " - " <> vcode
