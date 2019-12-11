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
    user <- (requireJsonBody :: Handler User)

    let user' = user {userRole = "brewmaster"}
    insertedComment <- runDB $ insertEntity user'
    returnJson $ insertedComment



postLoginR :: Handler Value
postLoginR = do
    user <- (requireJsonBody :: Handler LoginRequest)
    maybeUser <- runDB $ getBy $ UniqueUser (user_id user) --selectFirst [UserIdent ==. "mtirao"] []
    case maybeUser of
        Just user -> returnJson $ user
        Nothing -> notFound
    
