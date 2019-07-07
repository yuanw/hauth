{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}
module Adapter.InMemory.Auth where

import           ClassyPrelude
import           Data.Has
import qualified Domain.Auth as D
import           Text.StringRandom

type InMemory r m = (Has (TVar State) r, MonadReader r m, MonadIO m)

data State = State
    { stateAuths :: [(D.UserId, D.Auth)]
    , stateUnverifiedEmails :: Set D.Email
    , stateUserIdCounter :: Int
    , stateNotifications :: Map D.Email D.VerificationCode
    , stateSessions :: Map D.SessionId D.UserId
    } deriving (Show, Eq)


initialState :: State
initialState = State
    { stateAuths = mempty
    , stateUnverifiedEmails = mempty
    , stateUserIdCounter = 0
    , stateNotifications = mempty
    , stateSessions = mempty
    }

addAuth :: InMemory r m => D.Auth -> m (Either D.RegistrationError D.VerificationCode)
addAuth = undefined

setEmailAsVerified :: InMemory r m =>  D.VerificationCode -> m (Either D.EmailVerificationError ())
setEmailAsVerified = undefined

findUserByAuth :: InMemory r m => D.Auth -> m (Maybe (D.UserId, Bool))
findUserByAuth = undefined

findEmailFromUserId :: InMemory r m => D.UserId -> m (Maybe D.Email)
findEmailFromUserId uId = do
      tvar <- asks getter
      liftIO $ (D.authEmail <$>) . lookup uId . stateAuths <$> readTVarIO tvar

getNotificationsForEmail :: InMemory r m => D.Email -> m (Maybe D.VerificationCode)
getNotificationsForEmail email = do
    tvar <- asks getter
    liftIO $ lookup email . stateNotifications <$> readTVarIO tvar

notifyEmailVerification :: InMemory r m => D.Email -> D.VerificationCode -> m ()
notifyEmailVerification email code = do
    tvar <- asks getter
    atomically $ do
        state <- readTVar tvar
        let notificationMap = stateNotifications state
            newNotificationMap = insertMap email code notificationMap
            newState = state {stateNotifications = newNotificationMap}
        writeTVar tvar newState
    return ()

newSession :: InMemory r m => D.UserId -> m D.SessionId
newSession uId = do
    tvar <- asks getter
    sId <- liftIO $ (tshow uId <>) <$> stringRandomIO "[A-Za-z0-9]{16}"
    atomically $ do
        state <- readTVar tvar
        let sessions = stateSessions state
            newSessions = insertMap sId uId sessions
            newState = state {stateSessions = newSessions}
        writeTVar tvar newState
    return sId

findUserBySessionId :: InMemory r m => D.SessionId -> m (Maybe D.UserId)
findUserBySessionId sId = do
    tvar <- asks getter
    liftIO $ lookup sId . stateSessions <$> readTVarIO tvar
