{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Domain where

import Data.Text.Lazy
import Data.Text.Lazy.Encoding
import Data.Aeson
import Control.Applicative


data Article = Article Integer Text Text -- id title bodyText
     deriving (Show)

instance FromJSON Article where
     parseJSON (Object v) = Article <$>
                            v .:? "id" .!= 0 <*> -- the field "id" is optional
                            v .:  "title"    <*>
                            v .:  "bodyText"

instance ToJSON Article where
     toJSON (Article id title bodyText) =
         object ["id" .= id,
                 "title" .= title,
                 "bodyText" .= bodyText]

-- Request/Response Json
data Login = Login 
    { username :: Text
      , password :: Text
    }

instance ToJSON Login where
    toJSON Login {..} = object
        [
            "username" .= username,
            "password" .= password
        ]

instance FromJSON Login where
    parseJSON (Object v) = Login <$>
        v .:  "username" <*>
        v .:  "password"


-- Jus for testing                 
login = Login "fnisi@wannaplay.club" "3177AppL"


 --WS.get "/" do r <- liftIO $ NW.post "http://localhost:3000/accounts/login" (toJSON login)
             --               raw (r ^. responseBody)