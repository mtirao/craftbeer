{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE DeriveGeneric         #-}

module Handler.User where


import Import

data LoginRequest = LoginRequest 
    { user_id :: Text
    , password  :: Text
    }
    deriving (Show,Generic,Typeable) 
instance FromJSON LoginRequest
instance ToJSON LoginRequest


data LoginResponse = LoginResponse
    { message :: Text
    }
    deriving (Show,Generic) 
instance FromJSON LoginResponse
instance ToJSON LoginResponse


postUserR  :: Handler Value
postUserR = do 
    user <- (requireCheckJsonBody :: Handler User)
    let user' = user {userRole = "brewmaster"}
    insertedComment <- runDB $ insertEntity user'
    returnJson $ insertedComment

passwordRequest :: LoginRequest -> Text
passwordRequest (LoginRequest _ password) = password

passwordEntity:: User -> Text
passwordEntity (User _ _ password _) = case password of
                                        Just pwd -> pwd
                                        Nothing -> ""



postLoginR :: Handler Value
postLoginR = do
    user <- (requireCheckJsonBody :: Handler LoginRequest)
    maybeUser <- runDB $ getBy $ UniqueUser (user_id user) --selectFirst [UserIdent ==. "mtirao"] []
    case maybeUser of
        Just (Entity _ user') -> if (passwordRequest user) == (passwordEntity user')
                        then returnJson $ user'
                        else permissionDenied "Invalid password"
        Nothing -> notFound
    
