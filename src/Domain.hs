{-# LANGUAGE OverloadedStrings #-}

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
-- Error
data ErrorMessage = ErrorMessage Text
    deriving (Show)

instance ToJSON ErrorMessage where
    toJSON (ErrorMessage message) = object
        [
            "error" .= message
        ]

-- Login
data Login = Login Text Text -- username password
    deriving (Show)

instance ToJSON Login where
    toJSON (Login username password) = object
        [
            "username" .= username,
            "password" .= password
        ]

instance FromJSON Login where
    parseJSON (Object v) = Login <$>
        v .:  "username" <*>
        v .:  "password"

-- User
data User = User Text Text Text  Text Text -- password user name lastname role
     deriving (Show)

instance ToJSON User where
    toJSON (User password user name lastname role) = object
        [
            "user" .= user,
            "password" .= password,
            "name" .= name,
            "lastname" .= lastname,
            "role" .= role
        ]

instance FromJSON User where
    parseJSON (Object v) = User <$>
        v .:  "user" <*>
        v .:  "password" <*>
        v .:  "name" <*>
        v .:  "lastname" <*>
        v .:  "role"

-- Stages
data Stage = Stage Integer Integer Integer Integer
    deriving (Show)

instance ToJSON Stage where
    toJSON (Stage recipeid recipe_type temp time) = object
        [
            "recipeid" .= recipeid,
            "type" .= recipe_type,
            "temp" .= temp,
            "time" .= time
        ]

instance FromJSON Stage where
    parseJSON (Object v) = Stage <$>
        v .: "recipeid" .!= 0 <*>
        v .: "type" <*>
        v .: "temp" <*>
        v .: "time"

-- Sensosrs
data Sensor = Sensor Text Text Text
    deriving (Show)

instance ToJSON Sensor where
    toJSON (Sensor sensortype name file) = object
        [
            "type" .= sensortype,
            "name" .= name,
            "file" .= file
        ]


instance FromJSON Sensor where
    parseJSON (Object v) = Sensor <$>
        v .: "type" <*>
        v .: "name" <*>
        v .: "file"

-- Recipes
data Recipe = Recipe Text Text Integer Integer Integer
    deriving (Show)

instance ToJSON Recipe where
    toJSON (Recipe style name ibu abv color) = object
        [
            "style" .= style,
            "name" .= name,
            "ibu" .= ibu,
            "abv" .= abv,
            "color" .= color
        ]

instance FromJSON Recipe where
    parseJSON (Object v) = Recipe <$>
        v .: "style" <*>
        v .: "name" <*>
        v .: "ibu" <*>
        v .: "abv" <*>
        v .: "color"

-- Ingredient
data Ingredient = Ingredient Integer Text Text Integer
    deriving (Show)

instance ToJSON Ingredient where
    toJSON (Ingredient ingredientrecipe ingredientname ingredienttype unit) = object
        [
            "recipe" .= ingredientrecipe,
            "name" .= ingredientname,
            "type" .= ingredienttype,
            "unit" .= unit
        ]

instance FromJSON Ingredient where
    parseJSON (Object v) = Ingredient <$>
        v .: "recipe" <*>
        v .: "name" <*>
        v .: "type" <*>
        v .: "unit"

-- Agents
data Agent = Agent Text Text
    deriving (Show)

instance ToJSON Agent where
    toJSON (Agent agenttype ip) = object
        [
            "type" .= agenttype,
            "ip" .= ip
        ]

instance FromJSON Agent where
    parseJSON (Object v) = Agent <$>
        v .: "type" <*>
        v .: "ip"

 

-- Just for testing                 
login = Login "fnisi@wannaplay.club" "3177AppL"


 --WS.get "/" do r <- liftIO $ NW.post "http://localhost:3000/accounts/login" (toJSON login)
             --               raw (r ^. responseBody)