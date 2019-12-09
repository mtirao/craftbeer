{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE RecordWildCards       #-}

module Handler.User where

import Foundation
import Data.Text (Text)
import Yesod.Core

data User = User
    { first_name :: Text
    , last_name  :: Text
    , display_name :: Text
    , username  :: Text
    , status :: Maybe Text
    , token_type  :: Text
    , token :: Text
    }

instance ToJSON User where
    toJSON User {..} = object
        [ "first_name" .= first_name
        , "last_name" .= last_name
        , "display_name" .= display_name
        , "username" .= username
        , "status" .= status
        , "token_type" .= token_type
        , "token" .= token
        ]


postUserR  :: Handler Value
postUserR = returnJson $ User "Marcos" "Tirao" "Marcos Tirao" "marcos.tirao@icloud.com" Nothing "JWT" "12345677890"

