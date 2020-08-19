{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE RecordWildCards       #-}


module Domain where

import Data.Text.Lazy
import Data.Text.Lazy.Encoding
import Data.Aeson
import Control.Applicative
import GHC.Generics

-- Request Json
-- Error

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
            "username" .= user,
            "password" .= password,
            "name" .= name,
            "lastname" .= lastname,
            "role" .= role
        ]

instance FromJSON User where
    parseJSON (Object v) = User <$>
        v .:  "username" <*>
        v .:  "password" <*>
        v .:  "name" <*>
        v .:  "lastname" <*>
        v .:  "role"

-- Stages
data Stage = Stage{
    stageId:: Maybe Integer
    ,belong_to :: Integer
    ,stage_type :: Integer
    ,temp :: Integer
    ,time :: Integer
} deriving (Show, Generic)

instance ToJSON Stage
instance FromJSON Stage

-- Sensosrs
data Sensor = Sensor {
    sensorId :: Maybe Integer
    ,sensor_type :: Text
    ,name :: Text
    ,file :: Text
} deriving (Show, Generic)

instance ToJSON Sensor
instance FromJSON Sensor

-- Recipes
data Recipe = Recipe {
    recipeId :: Maybe Integer
    ,style :: Text
    ,recipe_name :: Text
    ,ibu :: Integer
    ,abv :: Integer
    ,color :: Integer
} deriving (Show, Generic)


instance FromJSON Recipe where
    parseJSON (Object v) = Recipe <$>
        v .:?  "id" <*>
        v .:  "style" <*>
        v .:  "name" <*>
        v .:  "ibu" <*>
        v .:  "abv" <*>
        v .:  "color"

instance ToJSON Recipe where
    toJSON Recipe {..} = object 
        [
            "id" .= recipeId
            ,"style" .= style
            ,"name" .= recipe_name
            ,"ibu" .= ibu
            ,"abv" .= abv
            ,"color" .= color
        ]


-- Ingredient
data Ingredient = Ingredient {
    ingredientId:: Maybe Integer
    ,recipe :: Integer
    ,ingredient_name :: Text
    ,ingredient_type :: Text
    ,unit :: Integer
} deriving (Show, Generic)

instance FromJSON Ingredient
instance ToJSON Ingredient

-- Agents
data Agent = Agent{
    agentId:: Maybe Integer
    ,agent_type :: Text
    ,ip :: Text
} deriving (Show, Generic)

instance FromJSON Agent
instance ToJSON Agent

--ErrorMessage
data ErrorMessage = ErrorMessage Text
    deriving (Show)

instance ToJSON ErrorMessage where
    toJSON (ErrorMessage message) = object
        [
            "error" .= message
        ]

--UserResponse
data UserResponse = UserResponse Text Text Text
     deriving (Show)

instance ToJSON UserResponse where 
    toJSON (UserResponse name lastname role) = object
        [
            "name" .= name,
            "lastname" .= lastname,
            "role" .= role
        ]

-- Getters
getUserName :: Maybe Login -> Text
getUserName a = case a of
                Nothing -> ""
                Just (Login u p) -> u   

getPassword :: Maybe Login -> Text
getPassword a = case a of
                Nothing -> ""
                Just (Login u p) -> p